package de.ume.deidentifhir.pii.openai

/** Decodes a token-by-token BIO(E)S label stream into character-aligned
 * [[Span]]s, with optional whitespace trimming and adjacent-span merging.
 *
 * Uses the HF "simple" aggregation strategy: collapse contiguous tokens whose
 * underlying category matches, regardless of B/I/E/S boundary tags. This is
 * what app-deidentify does and what the openai/privacy-filter model card
 * calls "good enough for an interactive demo". The model's constrained
 * Viterbi decoder produces sharper boundaries; that's a v2 concern.
 *
 * Many tokenizers (especially byte-level BPE like the one in this model)
 * return zero-width offsets `[X,X]` for a lot of tokens. We anchor each
 * span's character range to the FIRST and LAST tokens in the contiguous run
 * that have a non-zero-width offset. If none in the run have one, we fall
 * back to the surrounding tokens to bound the range.
 */
private[openai] object BioDecoder {

  /** A token classified by the model. `start`/`end` are character offsets in
   * the source text; either may be zero-width (`start == end`) for tokens the
   * tokenizer didn't track to a real text position. */
  final case class TokenSpan(start: Int, end: Int, label: String, score: Double)

  def decode(
      tokens: Seq[TokenSpan],
      sourceText: String,
      minScore: Double,
      trimWhitespace: Boolean,
      mergeAdjacent: Boolean
  ): Seq[Span] = {
    val raw = collectSpans(tokens, sourceText.length, minScore)
    val trimmed = if (trimWhitespace) raw.flatMap(trim(_, sourceText)) else raw
    if (mergeAdjacent) merge(trimmed, sourceText) else trimmed
  }

  /** Walk the token stream, opening a span at each category transition out of
   * `O`, closing it at the next transition. Score per span is the minimum
   * per-token score across the run. */
  private def collectSpans(
      tokens: Seq[TokenSpan],
      textLen: Int,
      minScore: Double
  ): Seq[Span] = {
    val out = scala.collection.mutable.ArrayBuffer.empty[Span]
    var spanCat: String = null
    var spanStart = -1
    var spanEnd = -1
    var spanScore = Double.PositiveInfinity

    val arr = tokens.toArray

    def flush(): Unit = {
      if (spanCat != null && spanStart >= 0 && spanEnd > spanStart && spanScore >= minScore) {
        out += Span(spanStart, spanEnd, spanCat, spanScore)
      }
      spanCat = null; spanStart = -1; spanEnd = -1; spanScore = Double.PositiveInfinity
    }

    var i = 0
    while (i < arr.length) {
      val t = arr(i)
      val cat = labelCategory(t.label)
      if (cat == null) {
        flush()
      } else if (spanCat == cat) {
        if (t.end > t.start) spanEnd = math.max(spanEnd, t.end)
        else if (spanEnd < 0) spanEnd = anchorEnd(arr, i, textLen)
        spanScore = math.min(spanScore, t.score)
      } else {
        flush()
        spanCat = cat
        spanStart = if (t.end > t.start) t.start else anchorStart(arr, i)
        spanEnd = if (t.end > t.start) t.end else anchorEnd(arr, i, textLen)
        spanScore = t.score
      }
      i += 1
    }
    flush()
    out.toSeq
  }

  /** Find the start offset for a zero-width token by looking at neighbors. */
  private def anchorStart(arr: Array[TokenSpan], i: Int): Int = {
    if (arr(i).start >= 0) arr(i).start
    else {
      var j = i - 1
      while (j >= 0 && !(arr(j).end > arr(j).start)) j -= 1
      if (j >= 0) arr(j).end else 0
    }
  }

  /** Find the end offset for a zero-width token by looking at neighbors. */
  private def anchorEnd(arr: Array[TokenSpan], i: Int, textLen: Int): Int = {
    var j = i + 1
    while (j < arr.length && !(arr(j).end > arr(j).start) && arr(j).start <= arr(i).start) j += 1
    if (j < arr.length) math.max(arr(j).start, arr(i).start) else math.max(arr(i).start, math.min(textLen, arr(i).end))
  }

  private def labelCategory(label: String): String = {
    if (label == null || label == "O") null
    else {
      val dash = label.indexOf('-')
      if (dash < 0) null else label.substring(dash + 1)
    }
  }

  /** Tighten span boundaries inward past whitespace. Returns None if nothing
   * non-whitespace remains. Ports app-deidentify's `_trim_whitespace`. */
  private def trim(s: Span, text: String): Option[Span] = {
    var st = s.start; var en = s.end
    while (st < en && Character.isWhitespace(text.charAt(st))) st += 1
    while (en > st && Character.isWhitespace(text.charAt(en - 1))) en -= 1
    if (st >= en) None else Some(Span(st, en, s.category, s.score))
  }

  /** Merge consecutive same-category spans separated by ≤ 1 whitespace char.
   * Ports app-deidentify's `_merge_adjacent`, which exists because HF "simple"
   * aggregation splits a single entity into adjacent spans when the model's
   * BIOES decoding (without the constrained Viterbi step) emits a B-tag in
   * the middle. */
  private def merge(spans: Seq[Span], text: String): Seq[Span] = {
    if (spans.isEmpty) return spans
    val sorted = spans.sortBy(_.start)
    val out = scala.collection.mutable.ArrayBuffer(sorted.head)
    for (s <- sorted.tail) {
      val cur = out.last
      val gap = text.substring(cur.end, s.start)
      if (s.category == cur.category && (s.start - cur.end) <= 1 && (gap.isEmpty || gap.forall(_.isWhitespace))) {
        out(out.length - 1) = Span(cur.start, s.end, cur.category, math.min(cur.score, s.score))
      } else {
        out += s
      }
    }
    out.toSeq
  }
}
