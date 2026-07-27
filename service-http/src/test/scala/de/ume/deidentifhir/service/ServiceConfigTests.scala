package de.ume.deidentifhir.service

import org.scalatest.funsuite.AnyFunSuite

class ServiceConfigTests extends AnyFunSuite {

  test("parse minimal config (no pii)") {
    val yaml =
      """service: { host: 127.0.0.1, port: 9090 }
        |profiles:
        |  default:
        |    paths:
        |      - { path: "Patient.id",   handler: passthrough }
        |      - { path: "Patient.name", handler: passthrough }
        |""".stripMargin
    val cfg = ServiceConfig.parse(yaml)
    assert(cfg.service.host == "127.0.0.1")
    assert(cfg.service.port == 9090)
    assert(cfg.piiOpenai.isEmpty)
    assert(cfg.profiles.keySet == Set("default"))
    assert(cfg.profiles("default").paths.size == 2)
    assert(!cfg.usesPiiOpenai, "no redact-pii references — usesPiiOpenai must be false")
  }

  test("server defaults applied when section omitted") {
    val cfg = ServiceConfig.parse(
      """profiles:
        |  default:
        |    paths:
        |      - { path: "Patient.id", handler: passthrough }
        |""".stripMargin)
    assert(cfg.service.host == "0.0.0.0")
    assert(cfg.service.port == 8080)
  }

  test("usesPiiOpenai detects redact-pii anywhere in any profile") {
    val cfg = ServiceConfig.parse(
      """profiles:
        |  audit:
        |    paths:
        |      - { path: "Patient.id", handler: passthrough }
        |  research:
        |    paths:
        |      - { path: "Observation.note.text", handler: redact-pii }
        |""".stripMargin)
    assert(cfg.usesPiiOpenai)
  }

  test("pii-openai block is parsed when present") {
    val cfg = ServiceConfig.parse(
      """pii-openai:
        |  cache-dir: /tmp/pii
        |  min-score: 0.7
        |  prewarm: true
        |profiles:
        |  default:
        |    paths:
        |      - { path: "Observation.note.text", handler: redact-pii }
        |""".stripMargin)
    assert(cfg.piiOpenai.exists(_.cacheDir.contains("/tmp/pii")))
    assert(cfg.piiOpenai.exists(_.minScore == 0.7))
    assert(cfg.piiOpenai.exists(_.prewarm))
  }

  test("handler-specific args go into PathSpec.args verbatim") {
    val cfg = ServiceConfig.parse(
      """profiles:
        |  default:
        |    paths:
        |      - { path: "Patient.birthDate", handler: date-shift, days: -7 }
        |      - { path: "Patient.name.family", handler: replace, value: "[redacted]" }
        |""".stripMargin)
    val paths = cfg.profiles("default").paths
    assert(paths.head.args.get("days").contains(-7))
    assert(paths(1).args.get("value").contains("[redacted]"))
  }

  test("missing 'profiles' fails loudly") {
    val ex = intercept[RuntimeException] {
      ServiceConfig.parse("""service: { port: 8080 }""")
    }
    assert(ex.getMessage.contains("profiles"))
  }

  test("path entry missing 'handler' fails with location") {
    val ex = intercept[RuntimeException] {
      ServiceConfig.parse(
        """profiles:
          |  default:
          |    paths:
          |      - { path: "Patient.id" }
          |""".stripMargin)
    }
    assert(ex.getMessage.contains("handler"), s"actual: ${ex.getMessage}")
    assert(ex.getMessage.contains("default"))
  }

  test("empty profiles map fails") {
    val ex = intercept[RuntimeException] {
      ServiceConfig.parse(
        """profiles: {}
          |""".stripMargin)
    }
    assert(ex.getMessage.contains("at least one"))
  }
}
