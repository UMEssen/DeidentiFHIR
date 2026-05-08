package de.ume.deidentifhir.pii.openai

import de.ume.deidentifhir.pii.openai.BioDecoder.TokenSpan
import org.scalatest.funsuite.AnyFunSuite

class BioDecoderTests extends AnyFunSuite {

  // Helper: T(s,e,label,score) — sane defaults so the call sites stay terse.
  private def T(start: Int, end: Int, label: String, score: Double = 0.99) =
    TokenSpan(start, end, label, score)

  test("decode single BIE span") {
    val text = "Hello John Doe."
    val tokens = Seq(
      T(0, 5, "O"),                       // Hello
      T(5, 6, "O"),                       // (space)
      T(6, 10, "B-private_person"),       // John
      T(10, 14, "E-private_person"),      // Doe (the trailing dot will be O)
      T(14, 15, "O")                      // .
    )
    val spans = BioDecoder.decode(tokens, text, 0.5, trimWhitespace = true, mergeAdjacent = true)
    assert(spans.size == 1)
    assert(spans.head.category == "private_person")
    assert(text.substring(spans.head.start, spans.head.end) == "John Doe")
  }

  test("decode single S-tag span") {
    val text = "Email me at x@y.z please"
    val tokens = Seq(
      T(0, 5, "O"),
      T(5, 8, "O"),
      T(8, 10, "O"),
      T(10, 12, "O"),
      T(12, 17, "S-private_email"),
      T(17, 24, "O")
    )
    val spans = BioDecoder.decode(tokens, text, 0.5, trimWhitespace = true, mergeAdjacent = true)
    assert(spans.size == 1)
    assert(spans.head.category == "private_email")
  }

  test("merge adjacent same-category spans across whitespace gap") {
    val text = "John Doe lives here"
    val tokens = Seq(
      T(0, 4, "B-private_person"),         // John
      T(4, 5, "O"),                        // (space)
      T(5, 8, "B-private_person"),         // Doe — broken aggregation: HF "simple" issues another B
      T(8, 19, "O")
    )
    val spans = BioDecoder.decode(tokens, text, 0.5, trimWhitespace = true, mergeAdjacent = true)
    assert(spans.size == 1, s"expected merge, got: $spans")
    assert(text.substring(spans.head.start, spans.head.end) == "John Doe")
  }

  test("do NOT merge adjacent spans of different categories") {
    val text = "John 1990-01-01"
    val tokens = Seq(
      T(0, 4, "S-private_person"),
      T(4, 5, "O"),
      T(5, 15, "S-private_date")
    )
    val spans = BioDecoder.decode(tokens, text, 0.5, trimWhitespace = true, mergeAdjacent = true)
    assert(spans.size == 2)
    assert(spans.map(_.category).toSet == Set("private_person", "private_date"))
  }

  test("minScore filter drops low-confidence spans") {
    val text = "Maybe this is a name"
    val tokens = Seq(
      T(0, 5, "O"),
      T(5, 6, "O"),
      T(6, 10, "S-private_person", score = 0.3),
      T(10, 20, "O")
    )
    val highCutoff = BioDecoder.decode(tokens, text, 0.5, trimWhitespace = true, mergeAdjacent = false)
    assert(highCutoff.isEmpty)
    val lowCutoff = BioDecoder.decode(tokens, text, 0.2, trimWhitespace = true, mergeAdjacent = false)
    assert(lowCutoff.size == 1)
  }

  test("trim leading whitespace from a span (byte-level BPE artifact)") {
    val text = "Patient John Doe."
    val tokens = Seq(
      T(0, 7, "O"),                        // Patient
      T(7, 12, "B-private_person"),        // " John" — leading space included
      T(12, 16, "E-private_person"),       // " Doe"
      T(16, 17, "O")
    )
    val spans = BioDecoder.decode(tokens, text, 0.5, trimWhitespace = true, mergeAdjacent = true)
    assert(spans.size == 1)
    assert(text.substring(spans.head.start, spans.head.end) == "John Doe")
  }

  test("zero-width offsets get anchored from neighbors") {
    val text = "John Doe lives here"
    val tokens = Seq(
      T(0, 0, "B-private_person"),
      T(4, 4, "I-private_person"),
      T(5, 8, "E-private_person"),
      T(8, 19, "O")
    )
    val spans = BioDecoder.decode(tokens, text, 0.5, trimWhitespace = true, mergeAdjacent = true)
    assert(spans.size == 1)
    val got = text.substring(spans.head.start, spans.head.end)
    assert(got.contains("Doe"))
  }

  test("empty token list yields empty span list") {
    assert(BioDecoder.decode(Nil, "anything", 0.5, trimWhitespace = true, mergeAdjacent = true).isEmpty)
  }

  test("all-O tokens yield empty span list") {
    val text = "Just a benign sentence."
    val tokens = (0 until text.length).map(i => T(i, i + 1, "O"))
    assert(BioDecoder.decode(tokens, text, 0.5, trimWhitespace = true, mergeAdjacent = true).isEmpty)
  }

  test("trim whitespace can wholly drop a span made of only whitespace") {
    val text = "Hello   world"
    val tokens = Seq(
      T(0, 5, "O"),
      T(5, 8, "S-private_person"),
      T(8, 13, "O")
    )
    val spans = BioDecoder.decode(tokens, text, 0.5, trimWhitespace = true, mergeAdjacent = false)
    assert(spans.isEmpty, s"whitespace-only span should be dropped, got: $spans")
  }

  test("loadLabels parses id2label from config.json") {
    val tmp = java.nio.file.Files.createTempFile("config", ".json")
    val json =
      """{
        |  "model_type": "test",
        |  "id2label": {
        |    "0": "O",
        |    "1": "B-private_person",
        |    "2": "I-private_person"
        |  }
        |}""".stripMargin
    java.nio.file.Files.writeString(tmp, json)
    try {
      val labels = PrivacyFilter.loadLabels(tmp)
      assert(labels.toSeq == Seq("O", "B-private_person", "I-private_person"))
    } finally java.nio.file.Files.deleteIfExists(tmp)
  }
}
