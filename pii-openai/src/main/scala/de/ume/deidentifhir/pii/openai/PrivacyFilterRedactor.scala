package de.ume.deidentifhir.pii.openai

import de.ume.deidentifhir.Deidentifhir.DeidentifhirHandler
import org.hl7.fhir.r4.model.{Base, StringType}

/** Thin synchronous adapter that lets a [[PrivacyFilter]] be plugged into
 * DeidentiFHIR's synchronous [[DeidentifhirHandler]] signature.
 *
 * Owns no resources of its own — the underlying [[PrivacyFilter]] holds the
 * model. Construct one PrivacyFilter at the top of your program, share it
 * across all redactors, and close it on shutdown.
 *
 * Use `asStringTypeHandler` against any HAPI `StringType`-valued FHIR path
 * (`*.text`, `Observation.note.text`, `DiagnosticReport.conclusion`, …):
 *
 * {{{
 *   val pf = PrivacyFilter()                                  // heavy
 *   val redactor = new PrivacyFilterRedactor(pf)              // light
 *   registry.addHander("redact-pii", redactor.asStringTypeHandler)
 * }}}
 *
 * The handler is registered against the four-argument DeidentifhirHandler
 * type (path, value, parents, static-context); the parent chain and context
 * map are unused — redaction depends only on the value's text.
 */
final class PrivacyFilterRedactor(filter: PrivacyFilter) {

  /** Sync `String => String` redaction. Returns the input unchanged if it's
   * null or empty (avoids a model call on trivial inputs). */
  def redact(s: String): String = filter.redact(s)

  /** Drop-in [[DeidentifhirHandler]] for HAPI [[StringType]] paths. */
  val asStringTypeHandler: DeidentifhirHandler[StringType] =
    (_: Seq[String], st: StringType, _: Seq[Base], _: Map[String, String]) => {
      val v = st.getValue
      if (v == null || v.isEmpty) st else new StringType(redact(v))
    }
}

object PrivacyFilterRedactor {
  def apply(filter: PrivacyFilter): PrivacyFilterRedactor = new PrivacyFilterRedactor(filter)
}
