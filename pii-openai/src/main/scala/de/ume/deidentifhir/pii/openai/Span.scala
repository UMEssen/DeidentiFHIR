package de.ume.deidentifhir.pii.openai

/** A detected PII span in a piece of text.
 *
 * @param start    inclusive character offset
 * @param end      exclusive character offset
 * @param category one of the eight openai/privacy-filter categories:
 *                 account_number, private_address, private_date,
 *                 private_email, private_person, private_phone, private_url,
 *                 secret. Kept as a String (not a sealed enum) so the model
 *                 can be upgraded with new labels without breaking callers.
 * @param score    minimum per-token score across the span, in [0, 1]
 */
final case class Span(start: Int, end: Int, category: String, score: Double) {
  def text(in: String): String = in.substring(start, end)
}
