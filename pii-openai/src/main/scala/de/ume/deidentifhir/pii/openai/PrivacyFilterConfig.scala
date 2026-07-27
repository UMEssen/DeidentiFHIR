package de.ume.deidentifhir.pii.openai

import java.nio.file.{Path, Paths}

/** Configuration for [[PrivacyFilter]].
 *
 * Sensible defaults: FP16 ONNX (the only variant that loads on stock ORT),
 * cache under `~/.cache/deidentifhir-pii/`, no mirror override. Override via
 * env vars `DEIDENTIFHIR_PII_CACHE_DIR` and `DEIDENTIFHIR_PII_MODEL_BASE_URL`
 * if you need to.
 *
 * @param cacheDir       where the model + tokenizer get downloaded on first
 *                       use; reused across processes. Default `~/.cache/deidentifhir-pii`.
 * @param modelBaseUrl   base URL the model files are fetched from. Default
 *                       points at the canonical HuggingFace repo. Override to
 *                       host a corporate mirror.
 * @param minScore       drop spans whose minimum per-token score is below
 *                       this threshold. The current implementation uses
 *                       softmax probabilities; 0.5 mirrors app-deidentify.
 * @param replacement    "auto" substitutes a category-based tag (e.g.
 *                       "[PERSON]"). Any other string is used verbatim.
 * @param trimWhitespace strip leading/trailing whitespace from each detected
 *                       span before redacting. Recommended (mirrors
 *                       app-deidentify's `_trim_whitespace`).
 * @param mergeAdjacent  merge consecutive same-category spans separated by
 *                       <= 1 whitespace char into one span before redacting.
 *                       Mirrors app-deidentify's `_merge_adjacent`. Mainly
 *                       useful when `useViterbi=false`; with Viterbi the
 *                       BIOES sequence is already structurally clean so
 *                       merging rarely changes anything.
 * @param useViterbi     if true (default), decode per-token labels with the
 *                       constrained Viterbi algorithm using the model's
 *                       `viterbi_calibration.json`. The model card describes
 *                       this as the canonical decode; per-token argmax can
 *                       and does produce structurally-illegal sequences. Set
 *                       to false only for diagnostic comparison.
 */
final case class PrivacyFilterConfig(
    cacheDir: Path = PrivacyFilterConfig.defaultCacheDir,
    modelBaseUrl: String = PrivacyFilterConfig.defaultBaseUrl,
    minScore: Double = 0.5,
    replacement: String = "auto",
    trimWhitespace: Boolean = true,
    mergeAdjacent: Boolean = true,
    useViterbi: Boolean = true
)

object PrivacyFilterConfig {
  val defaultBaseUrl: String =
    sys.env.getOrElse("DEIDENTIFHIR_PII_MODEL_BASE_URL",
      "https://huggingface.co/openai/privacy-filter/resolve/main/")

  val defaultCacheDir: Path = sys.env.get("DEIDENTIFHIR_PII_CACHE_DIR")
    .map(Paths.get(_))
    .getOrElse(Paths.get(System.getProperty("user.home"), ".cache", "deidentifhir-pii"))

  /** Convenience for Java callers. */
  def defaults: PrivacyFilterConfig = PrivacyFilterConfig()
}
