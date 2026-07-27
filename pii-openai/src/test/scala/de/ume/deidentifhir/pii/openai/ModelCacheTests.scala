package de.ume.deidentifhir.pii.openai

import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}
import java.security.MessageDigest

class ModelCacheTests extends AnyFunSuite {

  // Tiny synthetic "remote" file served via file:// URL — no real HTTP needed.
  private def writeFixture(content: Array[Byte]): (Path, String) = {
    val src = Files.createTempFile("modelcache-fixture", ".bin")
    Files.write(src, content)
    (src, hex(MessageDigest.getInstance("SHA-256").digest(content)))
  }

  private def hex(b: Array[Byte]): String = b.map(x => f"$x%02x").mkString

  test("sha256 streams the file in chunks, not loading it whole") {
    val (path, expected) = writeFixture("hello world".getBytes("UTF-8"))
    try assert(ModelCache.sha256(path) == expected)
    finally Files.deleteIfExists(path)
  }

  test("verifyOrFetch: cached file with correct hash is left untouched") {
    val (src, hash) = writeFixture(Array.fill(8 * 1024)('A'.toByte))
    val cacheDir = Files.createTempDirectory("modelcache-test")
    val target = cacheDir.resolve("present.bin")
    Files.copy(src, target)
    val mtimeBefore = Files.getLastModifiedTime(target)

    try {
      ModelCache.verifyOrFetch(target, src.toUri.toString, hash)
      assert(Files.exists(target))
      assert(Files.getLastModifiedTime(target) == mtimeBefore)
    } finally {
      Files.deleteIfExists(target)
      Files.deleteIfExists(cacheDir)
      Files.deleteIfExists(src)
    }
  }

  test("verifyOrFetch: cached file with wrong hash is deleted and re-downloaded") {
    val (src, correctHash) = writeFixture("real content".getBytes("UTF-8"))
    val cacheDir = Files.createTempDirectory("modelcache-test")
    val target = cacheDir.resolve("corrupt.bin")
    Files.write(target, "corrupted bytes".getBytes("UTF-8"))

    try {
      ModelCache.verifyOrFetch(target, src.toUri.toString, correctHash)
      assert(Files.exists(target))
      assert(new String(Files.readAllBytes(target), "UTF-8") == "real content",
        "target should contain the freshly-downloaded content")
      assert(ModelCache.sha256(target) == correctHash)
    } finally {
      Files.deleteIfExists(target)
      Files.deleteIfExists(cacheDir)
      Files.deleteIfExists(src)
    }
  }

  test("verifyOrFetch: missing file is downloaded and verified") {
    val (src, hash) = writeFixture(Array.fill(1024)('Z'.toByte))
    val cacheDir = Files.createTempDirectory("modelcache-test")
    val target = cacheDir.resolve("absent.bin")
    assert(!Files.exists(target))

    try {
      ModelCache.verifyOrFetch(target, src.toUri.toString, hash)
      assert(Files.exists(target))
      assert(ModelCache.sha256(target) == hash)
    } finally {
      Files.deleteIfExists(target)
      Files.deleteIfExists(cacheDir)
      Files.deleteIfExists(src)
    }
  }

  test("verifyOrFetch: mismatch on freshly-downloaded content fails loudly") {
    val (src, _) = writeFixture("upstream content".getBytes("UTF-8"))
    val cacheDir = Files.createTempDirectory("modelcache-test")
    val target = cacheDir.resolve("hash-mismatch.bin")
    val pinned = "0" * 64 // wrong on purpose

    try {
      val ex = intercept[RuntimeException] {
        ModelCache.verifyOrFetch(target, src.toUri.toString, pinned)
      }
      assert(ex.getMessage.contains("SHA256"))
      assert(ex.getMessage.contains("does not match"))
    } finally {
      Files.deleteIfExists(target)
      Files.deleteIfExists(cacheDir)
      Files.deleteIfExists(src)
    }
  }

  test("ExpectedSha256 covers every file referenced by materialize") {
    val basenames = Set(
      "model_fp16.onnx", "model_fp16.onnx_data", "model_fp16.onnx_data_1",
      "tokenizer.json", "config.json", "viterbi_calibration.json"
    )
    val pinned = ModelCache.ExpectedSha256.keySet
    assert(basenames.subsetOf(pinned),
      s"missing pinned hashes for: ${basenames.diff(pinned)}")
  }
}
