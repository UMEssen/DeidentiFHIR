package de.ume.deidentifhir.pii.openai

import ai.djl.Model
import ai.djl.huggingface.tokenizers.HuggingFaceTokenizer
import ai.djl.inference.Predictor
import ai.djl.ndarray.NDList
import ai.djl.ndarray.types.{DataType, Shape}
import ai.djl.translate.{Batchifier, Translator, TranslatorContext}
import com.typesafe.scalalogging.LazyLogging

import java.nio.file.{Files, Path}

/** Embedded openai/privacy-filter PII redactor. Loads the ONNX model in-process
 * via DJL + ONNX Runtime — no external sidecar needed.
 *
 * **Construction is cheap.** The constructor stores the config and returns
 * immediately; the model + tokenizer download (~2.85 GB to
 * `~/.cache/deidentifhir-pii`) and load (~3.5 GB RSS) happen on the first
 * `detect()` or `redact()` call. Configurations that pull in this artifact
 * but never invoke a redactor pay nothing at runtime.
 *
 * Call [[prewarm()]] to force eager init — useful when you want predictable
 * startup time in front of a k8s readiness probe instead of paying the load
 * cost on the first user request.
 *
 * Inference latency on CPU is ~70-180 ms per call depending on input length
 * (banded attention, so scaling is sub-linear).
 *
 * Thread safety: the wrapped DJL [[Predictor]] is not thread-safe under
 * concurrent calls. Either serialize access externally or create one
 * PrivacyFilter per worker. Keeping a single shared instance is the
 * recommended pattern; spawn multiple instances only after benchmarking
 * shows it actually improves throughput on your hardware.
 *
 * Always [[close()]] when done — the model holds native ONNX Runtime
 * resources that won't be reclaimed by the JVM GC. Close is a no-op if the
 * filter was never used.
 */
final class PrivacyFilter(val config: PrivacyFilterConfig)
    extends AutoCloseable with LazyLogging {

  // Mirror app-deidentify's category-tag mapping. Public so callers can
  // override the rendering for their locale.
  val categoryTag: Map[String, String] = PrivacyFilter.DefaultCategoryTag

  // Heavy resources are materialized lazily via DCL. We can't use `lazy val`
  // for the bundle because close() must be able to inspect "did init happen?"
  // without itself triggering init.
  @volatile private var coreOpt: Option[PrivacyFilter.Core] = None
  private val initLock: AnyRef = new Object

  private def core: PrivacyFilter.Core = coreOpt match {
    case Some(c) => c
    case None    => initLock.synchronized {
      coreOpt match {
        case Some(c) => c
        case None    =>
          logger.info("Loading openai/privacy-filter model on first use")
          val c = PrivacyFilter.Core.load(config)
          coreOpt = Some(c)
          c
      }
    }
  }

  /** Force model + tokenizer load now, instead of deferring to the first
   * `detect()`/`redact()` call. Idempotent. */
  def prewarm(): Unit = { val _ = core }

  /** True iff the model has been loaded (either via prewarm or a real call). */
  def isLoaded: Boolean = coreOpt.isDefined

  /** Detect PII spans in `text`. Returns one [[Span]] per detection, sorted
   * by start offset, with scores already filtered by `config.minScore`. */
  def detect(text: String): Seq[Span] = {
    if (text == null || text.isEmpty) return Seq.empty
    core.detect(text, config)
  }

  /** Redact PII spans in `text`, replacing each with the configured tag. */
  def redact(text: String): String = redact(text, config.replacement)

  /** Redact with a per-call replacement override. `"auto"` substitutes the
   * category tag (`[PERSON]`, `[EMAIL]`, …); any other string is used
   * verbatim (`***`, etc.). */
  def redact(text: String, replacement: String): String = {
    if (text == null || text.isEmpty) return text
    val spans = detect(text)
    if (spans.isEmpty) return text

    val out = new StringBuilder
    var cursor = 0
    for (s <- spans) {
      if (s.start >= cursor) {
        out.append(text.substring(cursor, s.start))
        out.append(if (replacement == "auto") s"[${categoryTag.getOrElse(s.category, s.category.toUpperCase)}]" else replacement)
        cursor = s.end
      }
    }
    out.append(text.substring(cursor))
    out.toString
  }

  /** Close held native resources. No-op if the filter was never used. */
  override def close(): Unit = initLock.synchronized {
    coreOpt match {
      case None => () // never initialized — nothing to release
      case Some(c) =>
        try c.close() catch { case e: Exception => logger.warn("close failed", e) }
        coreOpt = None
    }
  }
}

object PrivacyFilter {

  /** German category tags, matching app-deidentify's defaults. */
  val DefaultCategoryTag: Map[String, String] = Map(
    "account_number"  -> "KONTO",
    "private_address" -> "ADRESSE",
    "private_email"   -> "EMAIL",
    "private_person"  -> "PERSON",
    "private_phone"   -> "TELEFON",
    "private_url"     -> "URL",
    "private_date"    -> "DATUM",
    "secret"          -> "GEHEIMNIS"
  )

  def apply(): PrivacyFilter = new PrivacyFilter(PrivacyFilterConfig())
  def apply(config: PrivacyFilterConfig): PrivacyFilter = new PrivacyFilter(config)

  /** Parse `id2label` from `config.json` into an indexed array. Pure regex
   * parse to avoid dragging in a JSON dependency. */
  private[openai] def loadLabels(cfgPath: Path): Array[String] = {
    val raw = new String(Files.readAllBytes(cfgPath))
    val blockStart = raw.indexOf("\"id2label\"")
    if (blockStart < 0) sys.error(s"$cfgPath: missing id2label")
    val openBrace = raw.indexOf('{', blockStart)
    val closeBrace = raw.indexOf('}', openBrace)
    val block = raw.substring(openBrace + 1, closeBrace)
    val pat = """"(\d+)"\s*:\s*"([^"]+)"""".r
    val pairs = pat.findAllMatchIn(block).map(m => m.group(1).toInt -> m.group(2)).toMap
    if (pairs.isEmpty) sys.error(s"$cfgPath: id2label is empty")
    val n = pairs.keys.max + 1
    val arr = Array.fill(n)("O")
    pairs.foreach { case (i, l) => arr(i) = l }
    arr
  }

  /** Heavy bundle: model + tokenizer + decoder + DJL predictor. Encapsulated
   * so [[PrivacyFilter]] can defer construction (via DCL) and so close() can
   * cleanly tear down all of it without leaking. */
  private[openai] final class Core private (
      tokenizer: HuggingFaceTokenizer,
      labels: Array[String],
      viterbi: Option[ViterbiDecoder],
      model: Model,
      predictor: Predictor[Array[Long], Array[Array[Float]]]
  ) extends AutoCloseable {

    def detect(text: String, config: PrivacyFilterConfig): Seq[Span] = {
      val tokens = runModel(text)
      BioDecoder.decode(tokens, text, config.minScore, config.trimWhitespace, config.mergeAdjacent)
    }

    private def runModel(text: String): Seq[BioDecoder.TokenSpan] = {
      val enc = tokenizer.encode(text)
      val ids = enc.getIds
      val logits = predictor.predict(ids)
      val offsets = enc.getCharTokenSpans
      val n = logits.length

      // Phase 1: pick a label per token. Viterbi gives BIOES-legal sequences;
      // argmax fallback is for diagnostic comparison only.
      val labelIds: Array[Int] = viterbi match {
        case Some(v) => v.decode(logits)
        case None    => logits.map(argmax)
      }

      // Phase 2: per-token confidence = softmax probability of the chosen label.
      val out = new Array[BioDecoder.TokenSpan](n)
      var i = 0
      while (i < n) {
        val score = softmaxAt(logits(i), labelIds(i))
        val cs = if (i < offsets.length) offsets(i) else null
        val s = if (cs == null) -1 else cs.getStart
        val e = if (cs == null) -1 else cs.getEnd
        out(i) = BioDecoder.TokenSpan(s, e, labels(labelIds(i)), score)
        i += 1
      }
      out.toSeq
    }

    private def argmax(row: Array[Float]): Int = {
      var best = 0; var bestVal = Float.NegativeInfinity
      var i = 0
      while (i < row.length) { if (row(i) > bestVal) { bestVal = row(i); best = i }; i += 1 }
      best
    }

    private def softmaxAt(row: Array[Float], idx: Int): Double = {
      var maxV = Float.NegativeInfinity
      var i = 0
      while (i < row.length) { if (row(i) > maxV) maxV = row(i); i += 1 }
      var sumExp = 0.0
      i = 0
      while (i < row.length) { sumExp += math.exp(row(i) - maxV); i += 1 }
      math.exp(row(idx) - maxV) / sumExp
    }

    override def close(): Unit = {
      try predictor.close() catch { case _: Exception => () }
      try model.close()     catch { case _: Exception => () }
      try tokenizer.close() catch { case _: Exception => () }
    }
  }

  private[openai] object Core {
    /** Materialize all heavy resources. Triggers the model download if needed. */
    def load(config: PrivacyFilterConfig): Core = {
      val resolved = ModelCache.materialize(config)
      val tokenizer = HuggingFaceTokenizer.newInstance(resolved.tokenizerPath)
      val labels = PrivacyFilter.loadLabels(resolved.configPath)
      val viterbi =
        if (config.useViterbi) Some(new ViterbiDecoder(labels, ViterbiDecoder.loadBiases(resolved.calibrationPath)))
        else None
      val model = {
        val m = Model.newInstance("privacy-filter", "OnnxRuntime")
        m.load(resolved.modelPath)
        m
      }
      val predictor = model.newPredictor(new LogitsTranslator)
      new Core(tokenizer, labels, viterbi, model, predictor)
    }
  }

  /** DJL [[Translator]] from token-id array to logits. We bypass DJL's
   * batchifier because the model handles its own batch dimension via the
   * single-row reshape we do here. */
  private final class LogitsTranslator extends Translator[Array[Long], Array[Array[Float]]] {
    override def processInput(ctx: TranslatorContext, in: Array[Long]): NDList = {
      val mgr = ctx.getNDManager
      val ids = mgr.create(in).reshape(new Shape(1L, in.length.toLong)).toType(DataType.INT64, false)
      val mask = mgr.ones(new Shape(1L, in.length.toLong), DataType.INT64)
      new NDList(ids, mask)
    }

    override def processOutput(ctx: TranslatorContext, out: NDList): Array[Array[Float]] = {
      val arr = out.singletonOrThrow() // [1, seqLen, numLabels]
      val shape = arr.getShape.getShape
      val seqLen = shape(1).toInt
      val nLabels = shape(2).toInt
      val flat = arr.toFloatArray
      val rows = Array.ofDim[Float](seqLen, nLabels)
      var i = 0
      while (i < seqLen) {
        System.arraycopy(flat, i * nLabels, rows(i), 0, nLabels)
        i += 1
      }
      rows
    }

    override def getBatchifier: Batchifier = null
  }
}
