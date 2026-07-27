package de.ume.deidentifhir.pii.openai

import org.scalatest.funsuite.AnyFunSuite

class ViterbiDecoderTests extends AnyFunSuite {

  // Tiny synthetic label set so the tests are easy to reason about.
  private val labels: Array[String] = Array(
    "O",
    "B-person", "I-person", "E-person", "S-person",
    "B-email",  "I-email",  "E-email",  "S-email"
  )
  private val O = 0
  private val Bp = 1; private val Ip = 2; private val Ep = 3; private val Sp = 4
  private val Be = 5; /* private val Ie = 6; */ private val Ee = 7; private val Se = 8

  private val zeroBiases: Map[String, Double] = Map.empty

  private val v = new ViterbiDecoder(labels, zeroBiases)

  private def row(dominantLabel: Int, dominant: Float = 5f): Array[Float] = {
    val r = Array.fill(labels.length)(0f)
    r(dominantLabel) = dominant
    r
  }

  test("argmax-legal sequence is preserved unchanged") {
    val logits = Array(row(O), row(Bp), row(Ip), row(Ep), row(O))
    val decoded = v.decode(logits).toSeq
    assert(decoded == Seq(O, Bp, Ip, Ep, O))
  }

  test("S-tag single-token span is preserved") {
    val logits = Array(row(O), row(Sp), row(O))
    assert(v.decode(logits).toSeq == Seq(O, Sp, O))
  }

  test("forbids B-person -> I-email (cross-category continuation)") {
    val logits = Array(row(Bp, 10f), row(6, 10f), row(Ee, 10f))
    val decoded = v.decode(logits).toSeq
    assert(decoded.head != Bp || decoded(1) != 6,
      s"illegal cross-category transition: $decoded")
    assertLegal(decoded)
  }

  test("forbids B-X -> O (drop without close)") {
    val logits = Array(row(Bp, 10f), row(O, 10f), row(O, 10f))
    val decoded = v.decode(logits).toSeq
    assert(decoded.head != Bp || decoded(1) != O, s"illegal B->O: $decoded")
    assertLegal(decoded)
  }

  test("forbids I-X with no preceding B-X (illegal start)") {
    val logits = Array(row(Ip, 10f), row(Ep, 10f))
    val decoded = v.decode(logits).toSeq
    assert(decoded.head != Ip, s"I-X cannot start a sequence: $decoded")
    assertLegal(decoded)
  }

  test("forbids B-X as last token (illegal end)") {
    val logits = Array(row(O, 5f), row(Bp, 10f))
    val decoded = v.decode(logits).toSeq
    assert(decoded.last != Bp, s"B-X cannot end a sequence: $decoded")
    assertLegal(decoded)
  }

  test("rescues a legal middle even when argmax says O") {
    val mid = Array.fill(labels.length)(0f)
    mid(O) = 1.0f
    mid(Ip) = 0.5f
    val logits = Array(row(Bp, 10f), mid, row(Ep, 10f))
    val decoded = v.decode(logits).toSeq
    assert(decoded == Seq(Bp, Ip, Ep), s"expected rescue, got: $decoded")
  }

  test("two adjacent person spans separated by E,B handoff") {
    val logits = Array(row(Bp), row(Ep), row(Bp), row(Ep))
    val decoded = v.decode(logits).toSeq
    assert(decoded == Seq(Bp, Ep, Bp, Ep))
    assertLegal(decoded)
  }

  test("S followed by S is a legal handoff") {
    val logits = Array(row(Sp), row(Se))
    assert(v.decode(logits).toSeq == Seq(Sp, Se))
  }

  test("biases shift preference between equally-scoring legal paths") {
    val ambiguous = Array.fill(labels.length)(0f)
    ambiguous(O) = 1.0f
    ambiguous(Sp) = 1.0f
    val logits = Array(ambiguous, ambiguous, ambiguous)

    val preferStay = new ViterbiDecoder(labels,
      Map("transition_bias_background_stay" -> 5.0,
          "transition_bias_background_to_start" -> -5.0)).decode(logits).toSeq
    val preferStart = new ViterbiDecoder(labels,
      Map("transition_bias_background_stay" -> -5.0,
          "transition_bias_background_to_start" -> 5.0,
          "transition_bias_end_to_background" -> 5.0)).decode(logits).toSeq

    assert(preferStay.forall(_ == O), s"preferStay should be all O, got: $preferStay")
    assert(preferStart.contains(Sp), s"preferStart should pick S, got: $preferStart")
  }

  test("empty input yields empty output") {
    assert(v.decode(Array.empty).isEmpty)
  }

  // ---------- helpers

  private def labelType(i: Int): Char =
    if (labels(i) == "O") 'O' else labels(i).charAt(0)
  private def labelCat(i: Int): String =
    if (labels(i) == "O") null else labels(i).substring(labels(i).indexOf('-') + 1)

  private def assertLegal(seq: Seq[Int]): Unit = {
    assert(seq.nonEmpty)
    val first = labelType(seq.head)
    assert(first == 'O' || first == 'B' || first == 'S',
      s"illegal start type: ${labels(seq.head)}")
    val last = labelType(seq.last)
    assert(last == 'O' || last == 'E' || last == 'S',
      s"illegal end type: ${labels(seq.last)}")
    for (i <- 0 until seq.length - 1) {
      val ft = labelType(seq(i)); val tt = labelType(seq(i + 1))
      val fc = labelCat(seq(i));  val tc = labelCat(seq(i + 1))
      val ok = (ft, tt) match {
        case ('O', 'O' | 'B' | 'S')                            => true
        case ('B', 'I' | 'E') if fc == tc                      => true
        case ('I', 'I' | 'E') if fc == tc                      => true
        case ('E' | 'S', 'O' | 'B' | 'S')                       => true
        case _                                                  => false
      }
      assert(ok, s"illegal transition at $i: ${labels(seq(i))} -> ${labels(seq(i + 1))}")
    }
  }
}
