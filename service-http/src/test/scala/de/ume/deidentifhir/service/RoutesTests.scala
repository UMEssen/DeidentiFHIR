package de.ume.deidentifhir.service

import ca.uhn.fhir.context.FhirContext
import org.apache.pekko.http.scaladsl.model._
import org.apache.pekko.http.scaladsl.model.headers.RawHeader
import org.apache.pekko.http.scaladsl.testkit.ScalatestRouteTest
import org.hl7.fhir.r4.model.{Address, Patient}
import org.scalatest.funsuite.AnyFunSuite

class RoutesTests extends AnyFunSuite with ScalatestRouteTest {

  private val fhir = FhirContext.forR4()

  /** Build a registry using only handlers that don't need the model — keeps
   * route tests deterministic and offline. */
  private def buildRegistry(): ProfileRegistry = {
    val cfg = ServiceConfig.parse(
      """profiles:
        |  default:
        |    paths:
        |      - { path: "Patient",                       handler: passthrough }
        |      - { path: "Patient.gender",                handler: passthrough }
        |      - { path: "Patient.address",               handler: passthrough }
        |      - { path: "Patient.address.postalCode",    handler: replace, value: "00000" }
        |  audit:
        |    paths:
        |      - { path: "Patient",                       handler: passthrough }
        |      - { path: "Patient.gender",                handler: passthrough }
        |""".stripMargin)
    ProfileRegistry.build(cfg)
  }

  private def patientJson(): String = {
    val p = new Patient()
    p.setGender(org.hl7.fhir.r4.model.Enumerations.AdministrativeGender.FEMALE)
    p.addAddress(new Address().setPostalCode("12345").setCity("Springfield"))
    fhir.newJsonParser().encodeResourceToString(p)
  }

  test("POST /fhir/$deidentify with default profile applies the replace handler") {
    val reg = buildRegistry()
    try {
      val routes = new Routes(reg).routes
      val req = HttpRequest(
        method = HttpMethods.POST,
        uri = "/fhir/$deidentify",
        entity = HttpEntity(ContentTypes.`application/json`, patientJson())
      )
      req ~> routes ~> check {
        assert(status == StatusCodes.OK)
        val body = responseAs[String]
        assert(body.contains("\"00000\""), s"replace did not fire: $body")
        assert(body.contains("Patient"))
        assert(!body.contains("Springfield"))
      }
    } finally reg.close()
  }

  test("POST /fhir/Patient/$deidentify validates type") {
    val reg = buildRegistry()
    try {
      val routes = new Routes(reg).routes
      HttpRequest(
        method = HttpMethods.POST,
        uri = "/fhir/Observation/$deidentify",
        entity = HttpEntity(ContentTypes.`application/json`, patientJson())
      ) ~> routes ~> check {
        assert(status == StatusCodes.BadRequest)
        assert(responseAs[String].contains("expected Observation"))
      }
      HttpRequest(
        method = HttpMethods.POST,
        uri = "/fhir/Patient/$deidentify",
        entity = HttpEntity(ContentTypes.`application/json`, patientJson())
      ) ~> routes ~> check {
        assert(status == StatusCodes.OK)
      }
    } finally reg.close()
  }

  test("profile selection via ?profile= query parameter") {
    val reg = buildRegistry()
    try {
      val routes = new Routes(reg).routes
      HttpRequest(
        method = HttpMethods.POST,
        uri = "/fhir/$deidentify?profile=audit",
        entity = HttpEntity(ContentTypes.`application/json`, patientJson())
      ) ~> routes ~> check {
        assert(status == StatusCodes.OK)
        val body = responseAs[String]
        assert(!body.contains("00000"))
        assert(!body.contains("12345"))
        assert(body.contains("female"))
      }
    } finally reg.close()
  }

  test("profile selection via Prefer header") {
    val reg = buildRegistry()
    try {
      val routes = new Routes(reg).routes
      HttpRequest(
        method = HttpMethods.POST,
        uri = "/fhir/$deidentify",
        entity = HttpEntity(ContentTypes.`application/json`, patientJson()),
        headers = List(RawHeader("Prefer", "deidentify-profile=audit"))
      ) ~> routes ~> check {
        assert(status == StatusCodes.OK)
        val body = responseAs[String]
        assert(!body.contains("12345"), s"audit should drop address, got: $body")
      }
    } finally reg.close()
  }

  test("unknown profile returns 404 OperationOutcome") {
    val reg = buildRegistry()
    try {
      val routes = new Routes(reg).routes
      HttpRequest(
        method = HttpMethods.POST,
        uri = "/fhir/$deidentify?profile=does-not-exist",
        entity = HttpEntity(ContentTypes.`application/json`, patientJson())
      ) ~> routes ~> check {
        assert(status == StatusCodes.NotFound)
        val body = responseAs[String]
        assert(body.contains("OperationOutcome"))
        assert(body.contains("does-not-exist"))
      }
    } finally reg.close()
  }

  test("malformed FHIR JSON returns 400 OperationOutcome") {
    val reg = buildRegistry()
    try {
      val routes = new Routes(reg).routes
      HttpRequest(
        method = HttpMethods.POST,
        uri = "/fhir/$deidentify",
        entity = HttpEntity(ContentTypes.`application/json`, "{this is not fhir}")
      ) ~> routes ~> check {
        assert(status == StatusCodes.BadRequest)
        assert(responseAs[String].contains("OperationOutcome"))
      }
    } finally reg.close()
  }

  test("GET /fhir/metadata returns a CapabilityStatement") {
    val reg = buildRegistry()
    try {
      val routes = new Routes(reg).routes
      HttpRequest(method = HttpMethods.GET, uri = "/fhir/metadata") ~> routes ~> check {
        assert(status == StatusCodes.OK)
        val body = responseAs[String]
        assert(body.contains("\"resourceType\":\"CapabilityStatement\""))
        assert(body.contains("\"deidentify\""))
      }
    } finally reg.close()
  }

  test("GET /fhir/OperationDefinition/deidentify returns an OperationDefinition") {
    val reg = buildRegistry()
    try {
      val routes = new Routes(reg).routes
      HttpRequest(method = HttpMethods.GET, uri = "/fhir/OperationDefinition/deidentify") ~> routes ~> check {
        assert(status == StatusCodes.OK)
        val body = responseAs[String]
        assert(body.contains("\"resourceType\":\"OperationDefinition\""))
        assert(body.contains("\"code\":\"deidentify\""))
      }
    } finally reg.close()
  }

  test("GET /healthz and /readyz return 200") {
    val reg = buildRegistry()
    try {
      val routes = new Routes(reg).routes
      HttpRequest(method = HttpMethods.GET, uri = "/healthz") ~> routes ~> check {
        assert(status == StatusCodes.OK)
      }
      HttpRequest(method = HttpMethods.GET, uri = "/readyz") ~> routes ~> check {
        assert(status == StatusCodes.OK)
      }
    } finally reg.close()
  }
}
