package de.ume.deidentifhir.pii.openai

import com.typesafe.scalalogging.LazyLogging

import java.io.{BufferedInputStream, FileInputStream, FileOutputStream}
import java.net.URL
import java.nio.file.{Files, Path, StandardCopyOption}
import java.security.MessageDigest

/** Downloads and caches the model + tokenizer + config + Viterbi calibration
 * lazily on first use. Resumes via `.part` files (atomic rename on success).
 *
 * Each file is verified against a hard-coded SHA256 every time we use it
 * (cheap: ~1 GB/s on a modern CPU). Pinned hashes mean:
 *   - silent upstream changes can't swap weights under us;
 *   - a corrupted partial download is detected on next start;
 *   - reproducibility — the same hashes mean the same model behavior.
 *
 * To upgrade the model, recompute these hashes against the new upstream files.
 *
 * About ~2.85 GB total on first run (FP16 ONNX shards, the only variant that
 * loads on stock ONNX Runtime — INT8/INT4 use Microsoft contrib ops requiring
 * onnxruntime-extensions).
 */
private[openai] object ModelCache extends LazyLogging {

  /** Expected SHA256 hashes pinned to the openai/privacy-filter version we
   * tested against. Keyed by basename (the local filename in `cacheDir`). */
  val ExpectedSha256: Map[String, String] = Map(
    "model_fp16.onnx"          -> "04978ae127ae0922625319bfbf84b8664362ce32a1adc03414cf8215c3e2bc17",
    "model_fp16.onnx_data"     -> "744ef21b5311bf8f2e6500c4ef013a23d65277ac79a28679c0ddba964552f084",
    "model_fp16.onnx_data_1"   -> "77d85417c0317a920a13b8c3d21f58734287b6a1393bf1177a641f23996ccfd4",
    "tokenizer.json"           -> "0614fe83cadab421296e664e1f48f4261fa8fef6e03e63bb75c20f38e37d07d3",
    "config.json"              -> "b2b26a4a4a000639ad30b0c264adbefe365bdb567fbd7bb27303b8c438375bd1",
    "viterbi_calibration.json" -> "bbc8611ef08a55ed72d64856cbbbb9a91db8dfa881f0a92e2afbad6e4bbc775a"
  )

  // Remote paths (relative to config.modelBaseUrl). The ONNX shards must end up
  // next to model_fp16.onnx because ORT looks for the _data files relative to
  // the .onnx graph, so we deliberately strip the "onnx/" prefix locally.
  private val ModelFiles: Seq[String] = Seq(
    "onnx/model_fp16.onnx",
    "onnx/model_fp16.onnx_data",
    "onnx/model_fp16.onnx_data_1"
  )
  private val AuxFiles: Seq[String] = Seq(
    "tokenizer.json",
    "config.json",
    "viterbi_calibration.json"
  )

  /** Result of a successful cache materialization. */
  final case class Resolved(
      modelPath: Path,
      tokenizerPath: Path,
      configPath: Path,
      calibrationPath: Path
  )

  def materialize(config: PrivacyFilterConfig): Resolved = {
    if (!Files.exists(config.cacheDir)) Files.createDirectories(config.cacheDir)
    (ModelFiles ++ AuxFiles).foreach { remote =>
      val basename = remote.substring(remote.lastIndexOf('/') + 1)
      ensureFile(config.cacheDir.resolve(basename), config.modelBaseUrl + remote)
    }
    Resolved(
      modelPath       = config.cacheDir.resolve("model_fp16.onnx"),
      tokenizerPath   = config.cacheDir.resolve("tokenizer.json"),
      configPath      = config.cacheDir.resolve("config.json"),
      calibrationPath = config.cacheDir.resolve("viterbi_calibration.json")
    )
  }

  /** Resolve the pinned hash for a basename, or fail with a clear message. */
  private def ensureFile(target: Path, url: String): Unit = {
    val basename = target.getFileName.toString
    val expected = ExpectedSha256.getOrElse(basename, sys.error(s"no pinned SHA256 for $basename"))
    verifyOrFetch(target, url, expected)
  }

  /** Cached file with correct hash → returned. Cached file with wrong hash →
   * deleted + re-downloaded once. Missing → downloaded. Always verifies the
   * post-state. Throws if the freshly downloaded content still doesn't match.
   *
   * Exposed as `private[openai]` so tests can drive it without going through
   * the hard-coded [[ExpectedSha256]] map. */
  private[openai] def verifyOrFetch(target: Path, url: String, expectedSha256: String): Unit = {
    val basename = target.getFileName.toString

    if (Files.exists(target) && Files.size(target) > 0) {
      val actual = sha256(target)
      if (actual == expectedSha256) return
      logger.warn(s"$basename SHA256 mismatch (expected $expectedSha256, got $actual) — re-downloading")
      Files.delete(target)
    }

    download(target, url)
    val actual = sha256(target)
    if (actual != expectedSha256) {
      sys.error(s"$basename SHA256 $actual does not match pinned $expectedSha256 " +
        s"(downloaded from $url) — upstream may have changed; recompute hashes if so")
    }
  }

  private def download(target: Path, url: String): Unit = {
    logger.info(s"Downloading ${target.getFileName} from $url ...")
    val tmp = target.resolveSibling(target.getFileName.toString + ".part")
    val conn = new URL(url).openConnection()
    conn.setRequestProperty("User-Agent", "deidentifhir-pii-openai/1.0")
    val in = new BufferedInputStream(conn.getInputStream)
    val out = new FileOutputStream(tmp.toFile)
    try {
      val buf = new Array[Byte](1 << 16)
      var read = 0
      var total = 0L
      var lastReport = System.currentTimeMillis()
      while ({ read = in.read(buf); read > 0 }) {
        out.write(buf, 0, read)
        total += read
        val now = System.currentTimeMillis()
        if (now - lastReport > 5000) {
          logger.info(f"  ${target.getFileName}: ${total / 1e6}%.0f MB ...")
          lastReport = now
        }
      }
      logger.info(f"  ${target.getFileName}: ${total / 1e6}%.0f MB done")
    } finally {
      in.close(); out.close()
    }
    Files.move(tmp, target, StandardCopyOption.ATOMIC_MOVE)
  }

  /** Streaming SHA256. Doesn't load the file into memory — important for the
   * 2 GB ONNX shards. */
  private[openai] def sha256(path: Path): String = {
    val md = MessageDigest.getInstance("SHA-256")
    val in = new BufferedInputStream(new FileInputStream(path.toFile))
    try {
      val buf = new Array[Byte](1 << 16)
      var read = 0
      while ({ read = in.read(buf); read > 0 }) md.update(buf, 0, read)
    } finally in.close()
    md.digest().map(b => f"$b%02x").mkString
  }
}
