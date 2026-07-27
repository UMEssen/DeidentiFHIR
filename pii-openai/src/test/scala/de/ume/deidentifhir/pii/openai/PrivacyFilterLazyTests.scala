package de.ume.deidentifhir.pii.openai

import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}

/** Verifies that `PrivacyFilter` construction is cheap — i.e. it does NOT
 * trigger the 2.85 GB model download or the multi-second ONNX session load.
 *
 * Strategy: point the filter at a cache dir that will fail loudly the moment
 * the lazy model load is triggered (mirror URL pointing into nowhere, empty
 * cache). If construction or `isLoaded` accidentally pulled the lazy chain,
 * we'd see download attempts to the bogus URL and the test would fail with
 * an IOException. As long as nothing initializes, we never touch the URL.
 *
 * This is a regression test for the lazy-load contract: any future refactor
 * that re-eagerly initializes will break this test before it ships.
 */
class PrivacyFilterLazyTests extends AnyFunSuite {

  private def freshConfig(): (Path, PrivacyFilterConfig) = {
    val cache = Files.createTempDirectory("pf-lazy-test")
    val cfg = PrivacyFilterConfig(
      cacheDir = cache,
      modelBaseUrl = "https://this-host-must-not-exist.invalid/"
    )
    (cache, cfg)
  }

  test("construction does not load the model") {
    val (cache, cfg) = freshConfig()
    try {
      val pf = new PrivacyFilter(cfg)
      try {
        assert(!pf.isLoaded, "constructor must not trigger model load")
        val entries =
          if (Files.exists(cache)) Files.list(cache).toArray.toSeq else Nil
        assert(entries.isEmpty, s"cache should be empty after construction, got: $entries")
      } finally pf.close()
    } finally Files.deleteIfExists(cache)
  }

  test("close is a no-op when never used") {
    val (cache, cfg) = freshConfig()
    try {
      val pf = new PrivacyFilter(cfg)
      pf.close()                              // would throw if it forced init
      assert(!pf.isLoaded)
      pf.close()                              // double close is also fine
    } finally Files.deleteIfExists(cache)
  }

  test("redact on null/empty input does not load the model") {
    val (cache, cfg) = freshConfig()
    try {
      val pf = new PrivacyFilter(cfg)
      try {
        assert(pf.redact(null) == null)
        assert(pf.redact("") == "")
        assert(!pf.isLoaded, "trivial inputs must short-circuit before lazy init")
      } finally pf.close()
    } finally Files.deleteIfExists(cache)
  }

  test("apply() factory does not load the model") {
    val (cache, cfg) = freshConfig()
    try {
      val pf = PrivacyFilter(cfg)
      try assert(!pf.isLoaded)
      finally pf.close()
    } finally Files.deleteIfExists(cache)
  }

  test("isLoaded flips to true once init succeeds (integration)") {
    val enabled = sys.props.get("deidentifhir.pii.integration").contains("1") ||
      sys.env.get("DEIDENTIFHIR_PII_INTEGRATION").contains("1")
    assume(enabled, "set -Ddeidentifhir.pii.integration=1 to run")

    val pf = PrivacyFilter()
    try {
      assert(!pf.isLoaded)
      pf.prewarm()
      assert(pf.isLoaded)
      pf.prewarm()
      assert(pf.isLoaded)
    } finally pf.close()
  }
}
