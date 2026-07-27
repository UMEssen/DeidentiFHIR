package de.ume.deidentifhir.pii.openai

import org.hl7.fhir.r4.model.{Base, StringType}
import org.scalatest.funsuite.AnyFunSuite

/** End-to-end test that loads the real openai/privacy-filter ONNX model and
 * runs a few representative redactions through it.
 *
 * Gated by the system property `deidentifhir.pii.integration` because:
 *   - the model + tokenizer is ~2.85 GB, downloaded on first run;
 *   - resident memory after load is ~3.5 GB.
 *
 * Run with:
 *   mvn -pl pii-openai test -Ddeidentifhir.pii.integration=1
 *
 * Without the property, tests cancel cleanly so plain `mvn test` stays fast.
 */
class PrivacyFilterIntegrationTests extends AnyFunSuite {

  private val Enabled =
    sys.props.get("deidentifhir.pii.integration").contains("1") ||
      sys.env.get("DEIDENTIFHIR_PII_INTEGRATION").contains("1")

  private lazy val pf: PrivacyFilter = PrivacyFilter()

  override def withFixture(test: NoArgTest): org.scalatest.Outcome = {
    assume(Enabled, "set -Ddeidentifhir.pii.integration=1 to run")
    super.withFixture(test)
  }

  test("redact English: name, address, email") {
    val out = pf.redact("John Doe lives at 123 Main Street, Springfield. Email: john@example.com.")
    assert(out.contains("[PERSON]"), s"expected PERSON tag, got: $out")
    assert(out.contains("[ADRESSE]"), s"expected ADRESSE tag, got: $out")
    assert(out.contains("[EMAIL]"), s"expected EMAIL tag, got: $out")
    assert(!out.contains("John Doe"), s"name leaked, got: $out")
    assert(!out.contains("john@example.com"), s"email leaked, got: $out")
  }

  test("redact German: name, dates, phone, email, account") {
    val text = "Patient Mr. Maximilian Mustermann, born 1973-08-04, was admitted on 2024-03-12. " +
      "Contact: +49 30 1234 5678 or m.mustermann@example.de. Insurance ID: AOK-883472291."
    val out = pf.redact(text)
    assert(out.contains("[PERSON]"))
    assert(out.contains("[DATUM]"))
    assert(out.contains("[TELEFON]"))
    assert(out.contains("[EMAIL]"))
    assert(out.contains("[KONTO]"))
    assert(!out.contains("Maximilian Mustermann"))
    assert(!out.contains("m.mustermann@example.de"))
    assert(!out.contains("+49 30 1234 5678"))
  }

  test("benign text passes through untouched") {
    val text = "The quick brown fox jumps over the lazy dog."
    assert(pf.redact(text) == text)
  }

  test("detect returns spans with categories and scores") {
    val spans = pf.detect("John Doe lives at 123 Main Street.")
    assert(spans.nonEmpty)
    assert(spans.exists(_.category == "private_person"))
    assert(spans.forall(s => s.score >= 0.5 && s.score <= 1.0))
  }

  test("StringType handler integrates with the DeidentifhirHandler signature") {
    val redactor = new PrivacyFilterRedactor(pf)
    val handler = redactor.asStringTypeHandler
    val out = handler(
      Seq("Observation", "note", "text"),
      new StringType("Visit by Dr. Smith on 2024-03-12."),
      Seq.empty[Base],
      Map.empty[String, String]
    )
    assert(out.getValue.contains("[PERSON]") || out.getValue.contains("[DATUM]"),
      s"expected at least one tag, got: ${out.getValue}")
  }

  test("null/empty StringType passes through unchanged without a model call") {
    val redactor = new PrivacyFilterRedactor(pf)
    val handler = redactor.asStringTypeHandler
    assert(handler(Seq("x"), new StringType(), Seq.empty[Base], Map.empty[String, String]).getValue == null)
    val empty = new StringType("")
    val out = handler(Seq("x"), empty, Seq.empty[Base], Map.empty[String, String])
    assert(out.getValue == "")
  }
}
