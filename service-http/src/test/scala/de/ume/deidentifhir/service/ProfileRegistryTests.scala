package de.ume.deidentifhir.service

import org.hl7.fhir.r4.model.{Address, Patient}
import org.scalatest.funsuite.AnyFunSuite

class ProfileRegistryTests extends AnyFunSuite {

  /** Without a redact-pii reference, no PrivacyFilter must be constructed —
   * which is the whole point of the lazy-init plumbing. */
  test("no PrivacyFilter is constructed when no profile references redact-pii") {
    val cfg = ServiceConfig.parse(
      """profiles:
        |  default:
        |    paths:
        |      - { path: "Patient.id",        handler: passthrough }
        |      - { path: "Patient.gender",    handler: passthrough }
        |""".stripMargin)
    val reg = ProfileRegistry.build(cfg)
    try assert(reg.privacyFilter.isEmpty,
      "redact-pii not referenced — PrivacyFilter must NOT be instantiated")
    finally reg.close()
  }

  test("invalid handler name fails at build time, not at request time") {
    val cfg = ServiceConfig.parse(
      """profiles:
        |  default:
        |    paths:
        |      - { path: "Patient.id", handler: nonexistent }
        |""".stripMargin)
    val ex = intercept[RuntimeException] { ProfileRegistry.build(cfg) }
    assert(ex.getMessage.contains("nonexistent"))
  }

  test("passthrough preserves the configured field") {
    val cfg = ServiceConfig.parse(
      """profiles:
        |  default:
        |    paths:
        |      - { path: "Patient",                       handler: passthrough }
        |      - { path: "Patient.gender",                handler: passthrough }
        |      - { path: "Patient.address",               handler: passthrough }
        |      - { path: "Patient.address.postalCode",    handler: passthrough }
        |""".stripMargin)
    val reg = ProfileRegistry.build(cfg)
    try {
      val p = new Patient()
      p.setGender(org.hl7.fhir.r4.model.Enumerations.AdministrativeGender.FEMALE)
      p.addAddress(new Address().setPostalCode("12345"))
      val out = reg.get("default").get.deidentify(p).asInstanceOf[Patient]
      assert(out.getGender.toCode == "female")
      assert(out.getAddress.size() == 1)
      assert(out.getAddress.get(0).getPostalCode == "12345")
    } finally reg.close()
  }

  test("sha256 hashes string fields") {
    val cfg = ServiceConfig.parse(
      """profiles:
        |  default:
        |    paths:
        |      - { path: "Patient",                       handler: passthrough }
        |      - { path: "Patient.address",               handler: passthrough }
        |      - { path: "Patient.address.postalCode",    handler: sha256 }
        |""".stripMargin)
    val reg = ProfileRegistry.build(cfg)
    try {
      val p = new Patient()
      p.addAddress(new Address().setPostalCode("12345"))
      val out = reg.get("default").get.deidentify(p).asInstanceOf[Patient]
      val pc = out.getAddress.get(0).getPostalCode
      assert(pc != "12345")
      assert(pc.length == 64, s"sha256 hex is 64 chars, got: $pc")
    } finally reg.close()
  }

  test("replace substitutes a literal string for the field") {
    val cfg = ServiceConfig.parse(
      """profiles:
        |  default:
        |    paths:
        |      - { path: "Patient",                       handler: passthrough }
        |      - { path: "Patient.address",               handler: passthrough }
        |      - { path: "Patient.address.postalCode",    handler: replace, value: "00000" }
        |""".stripMargin)
    val reg = ProfileRegistry.build(cfg)
    try {
      val p = new Patient()
      p.addAddress(new Address().setPostalCode("12345"))
      val out = reg.get("default").get.deidentify(p).asInstanceOf[Patient]
      assert(out.getAddress.get(0).getPostalCode == "00000")
    } finally reg.close()
  }

  test("date-shift moves a BaseDateTimeType by the configured days") {
    val cfg = ServiceConfig.parse(
      """profiles:
        |  default:
        |    paths:
        |      - { path: "Patient",            handler: passthrough }
        |      - { path: "Patient.birthDate",  handler: date-shift, days: -7 }
        |""".stripMargin)
    val reg = ProfileRegistry.build(cfg)
    try {
      val p = new Patient()
      p.setBirthDateElement(new org.hl7.fhir.r4.model.DateType("2024-01-15"))
      val out = reg.get("default").get.deidentify(p).asInstanceOf[Patient]
      assert(out.getBirthDateElement.getValueAsString == "2024-01-08")
    } finally reg.close()
  }

  test("unknown profile lookup returns None") {
    val cfg = ServiceConfig.parse(
      """profiles:
        |  default:
        |    paths:
        |      - { path: "Patient.id", handler: passthrough }
        |""".stripMargin)
    val reg = ProfileRegistry.build(cfg)
    try {
      assert(reg.get("does-not-exist").isEmpty)
      assert(reg.get("default").isDefined)
    } finally reg.close()
  }
}
