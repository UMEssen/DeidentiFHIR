package de.ume.deidentifhir.pii.openai

/** Constrained linear-chain Viterbi decoder for BIOES token classification.
 *
 * Given per-token emission logits over the model's 33 labels, produces the
 * highest-scoring label sequence subject to BIOES structural constraints:
 *
 *   - O can transition to: O, B-X, S-X (any X)
 *   - B-X can transition to: I-X, E-X (same category only)
 *   - I-X can transition to: I-X, E-X (same category only)
 *   - E-X / S-X can transition to: O, B-Y, S-Y (any Y, including same)
 *
 * Forbidden transitions (e.g. B-X → I-Y, B-X → O, I-X → O) get -inf scores so
 * the algorithm never produces malformed sequences. Argmax-per-token, in
 * contrast, can and does produce them, which is the whole reason this exists.
 *
 * Six learnable transition biases from the model's `viterbi_calibration.json`
 * are added to the relevant allowed transitions; for `openai/privacy-filter`
 * v1 they all default to 0.0, so the practical effect is purely structural
 * filtering. Future model versions may train non-zero biases.
 *
 * Implementation note: dense N×N transition matrix with -inf for forbidden
 * cells. For N=33 and T~1500 tokens that's ~1.6M float ops per decode —
 * sub-millisecond on CPU and dwarfed by model inference.
 */
private[openai] final class ViterbiDecoder(labels: Array[String], biases: Map[String, Double]) {

  private val NegInf: Float = Float.NegativeInfinity
  private val n: Int = labels.length

  // Pre-compute everything that's a function of (labels, biases) so that
  // decode(logits) is hot-loop-only.
  private val transition: Array[Array[Float]] = buildTransition()
  private val startMask: Array[Float] = buildStartMask()
  private val endMask: Array[Float] = buildEndMask()

  /** Decode the optimal BIOES-legal label sequence for these per-token
   * emission scores. Input shape: [seqLen][numLabels]. Output: [seqLen]. */
  def decode(logits: Array[Array[Float]]): Array[Int] = {
    val t = logits.length
    if (t == 0) return Array.emptyIntArray

    val forwardCur = new Array[Float](n)
    val forwardPrev = new Array[Float](n)
    val backptr = Array.ofDim[Int](t, n)

    var l = 0
    while (l < n) {
      forwardPrev(l) = logits(0)(l) + startMask(l)
      backptr(0)(l) = -1
      l += 1
    }

    var step = 1
    while (step < t) {
      var lt = 0
      while (lt < n) {
        var best = NegInf
        var bestFrom = 0
        var lf = 0
        while (lf < n) {
          val tr = transition(lf)(lt)
          if (tr != NegInf) {
            val s = forwardPrev(lf) + tr
            if (s > best) { best = s; bestFrom = lf }
          }
          lf += 1
        }
        forwardCur(lt) = best + logits(step)(lt)
        backptr(step)(lt) = bestFrom
        lt += 1
      }
      System.arraycopy(forwardCur, 0, forwardPrev, 0, n)
      step += 1
    }

    var endBest = NegInf
    var endIdx = 0
    l = 0
    while (l < n) {
      val s = forwardPrev(l) + endMask(l)
      if (s > endBest) { endBest = s; endIdx = l }
      l += 1
    }

    val out = new Array[Int](t)
    out(t - 1) = endIdx
    var i = t - 2
    while (i >= 0) {
      out(i) = backptr(i + 1)(out(i + 1))
      i -= 1
    }
    out
  }

  // ---------- builders

  private def buildTransition(): Array[Array[Float]] = {
    val m = Array.fill(n, n)(NegInf)
    var i = 0
    while (i < n) {
      var j = 0
      while (j < n) {
        biasForOpt(labels(i), labels(j)) match {
          case Some(b) => m(i)(j) = b.toFloat
          case None    => () // stays -inf
        }
        j += 1
      }
      i += 1
    }
    m
  }

  private def buildStartMask(): Array[Float] = {
    val m = new Array[Float](n)
    var i = 0
    while (i < n) {
      m(i) = labelType(labels(i)) match {
        case 'O' | 'B' | 'S' => 0.0f
        case _               => NegInf
      }
      i += 1
    }
    m
  }

  private def buildEndMask(): Array[Float] = {
    val m = new Array[Float](n)
    var i = 0
    while (i < n) {
      m(i) = labelType(labels(i)) match {
        case 'O' | 'E' | 'S' => 0.0f
        case _               => NegInf
      }
      i += 1
    }
    m
  }

  private def labelType(label: String): Char =
    if (label == "O") 'O' else label.charAt(0)

  private def labelCategory(label: String): String =
    if (label == "O") null
    else {
      val dash = label.indexOf('-')
      if (dash < 0) null else label.substring(dash + 1)
    }

  /** Bias for `from -> to` if the transition is BIOES-legal, else None. */
  private def biasForOpt(from: String, to: String): Option[Double] = {
    val ft = labelType(from); val tt = labelType(to)
    val fc = labelCategory(from); val tc = labelCategory(to)
    val key: String = (ft, tt) match {
      case ('O', 'O')                                => "transition_bias_background_stay"
      case ('O', 'B') | ('O', 'S')                   => "transition_bias_background_to_start"
      case ('B', 'I') | ('I', 'I') if fc == tc       => "transition_bias_inside_to_continue"
      case ('B', 'E') | ('I', 'E') if fc == tc       => "transition_bias_inside_to_end"
      case ('E', 'O') | ('S', 'O')                   => "transition_bias_end_to_background"
      case ('E', 'B') | ('E', 'S')                   => "transition_bias_end_to_start"
      case ('S', 'B') | ('S', 'S')                   => "transition_bias_end_to_start"
      case _                                          => return None
    }
    Some(biases.getOrElse(key, 0.0))
  }
}

private[openai] object ViterbiDecoder {

  /** Parse `viterbi_calibration.json` and return the `default` operating-point
   * biases as a flat Map[String, Double]. Pure regex parse, kept consistent
   * with `PrivacyFilter.loadLabels` so we don't need a JSON dep. */
  def loadBiases(calibrationPath: java.nio.file.Path): Map[String, Double] = {
    val raw = new String(java.nio.file.Files.readAllBytes(calibrationPath))
    val bIdx = raw.indexOf("\"biases\"")
    if (bIdx < 0) sys.error(s"$calibrationPath: missing biases block")
    val openBrace = raw.indexOf('{', bIdx)
    val closeBrace = raw.indexOf('}', openBrace)
    val block = raw.substring(openBrace + 1, closeBrace)
    val pat = """"([A-Za-z_]+)"\s*:\s*(-?\d+(?:\.\d+)?(?:[eE][-+]?\d+)?)""".r
    pat.findAllMatchIn(block).map(m => m.group(1) -> m.group(2).toDouble).toMap
  }
}
