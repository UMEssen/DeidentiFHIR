package de.ume.deidentifhir.service

import ca.uhn.fhir.context.FhirContext
import ca.uhn.fhir.parser.DataFormatException
import com.typesafe.scalalogging.LazyLogging
import org.apache.pekko.http.scaladsl.model._
import org.apache.pekko.http.scaladsl.server.Directives._
import org.apache.pekko.http.scaladsl.server.Route
import org.hl7.fhir.r4.model.{CapabilityStatement, OperationDefinition, Resource}

/** HTTP route surface for the FHIR `$deidentify` operation.
 *
 * Endpoints:
 *   - `POST /fhir/$deidentify`                  — system-level
 *   - `POST /fhir/{ResourceType}/$deidentify`   — type-level (asserts the
 *                                                 incoming resource matches)
 *   - `GET  /fhir/OperationDefinition/deidentify` — operation discovery
 *   - `GET  /fhir/metadata`                     — minimal CapabilityStatement
 *   - `GET  /healthz`                           — liveness
 *   - `GET  /readyz`                            — readiness
 *
 * Profile selection: query `?profile=<name>` or header
 * `Prefer: deidentify-profile=<name>`, defaulting to `default`.
 *
 * Content negotiation is intentionally minimal: bodies are parsed as
 * `application/fhir+json` (or `application/json`); responses are JSON. XML
 * support would mean swapping HAPI parsers — out of scope for v1.
 */
final class Routes(registry: ProfileRegistry) extends LazyLogging {

  private val fhirContext = FhirContext.forR4()
  private val FhirJson: ContentType.WithFixedCharset =
    MediaType.applicationWithFixedCharset("fhir+json", HttpCharsets.`UTF-8`).toContentType

  private val DefaultProfile = "default"

  /** Top-level route — call from the Main pekko-http binding. */
  def routes: Route = concat(
    pathPrefix("fhir") {
      concat(
        // Discovery first (more specific) so it's not shadowed by the
        // catch-all type-level prefix below.
        path("metadata") {
          get { complete(jsonResponse(capabilityStatement)) }
        },
        path("OperationDefinition" / "deidentify") {
          get { complete(jsonResponse(operationDefinition)) }
        },
        // System-level operation
        path("$deidentify") {
          post { withProfile(deidentifySystem) }
        },
        // Type-level operation: /fhir/Patient/$deidentify, /fhir/Bundle/$deidentify, …
        pathPrefix(Segment) { resourceType =>
          path("$deidentify") {
            post { withProfile(deidentifyType(resourceType)) }
          }
        }
      )
    },
    // Probes
    path("healthz") { get { complete(StatusCodes.OK -> "ok") } },
    path("readyz")  { get { complete(readyResponse) } }
  )

  // ---------- handlers

  private def deidentifySystem(profileName: String): Route =
    entity(as[String]) { body => complete(runDeidentify(body, profileName, expected = None)) }

  private def deidentifyType(resourceType: String)(profileName: String): Route =
    entity(as[String]) { body => complete(runDeidentify(body, profileName, expected = Some(resourceType))) }

  private def runDeidentify(body: String, profileName: String, expected: Option[String]): HttpResponse = {
    registry.get(profileName) match {
      case None =>
        operationOutcome(StatusCodes.NotFound, "not-found",
          s"Unknown profile '$profileName'. Configured: ${registry.profileNames.mkString(", ")}")
      case Some(deid) =>
        try {
          val parser = fhirContext.newJsonParser()
          parser.parseResource(body) match {
            case r: Resource =>
              expected match {
                case Some(exp) if r.fhirType() != exp =>
                  operationOutcome(StatusCodes.BadRequest, "invariant",
                    s"Type-level $$deidentify expected $exp, got ${r.fhirType()}")
                case _ =>
                  val out = deid.deidentify(r)
                  HttpResponse(StatusCodes.OK, entity = HttpEntity(FhirJson, parser.encodeResourceToString(out)))
              }
            case other =>
              operationOutcome(StatusCodes.BadRequest, "structure",
                s"Expected a FHIR Resource, got ${other.fhirType()}")
          }
        } catch {
          case e: DataFormatException =>
            operationOutcome(StatusCodes.BadRequest, "invalid", s"Failed to parse FHIR JSON: ${e.getMessage}")
          case e: Exception =>
            logger.warn("deidentify failed", e)
            operationOutcome(StatusCodes.InternalServerError, "exception",
              s"${e.getClass.getSimpleName}: ${e.getMessage}")
        }
    }
  }

  // ---------- profile selection

  private def withProfile(inner: String => Route): Route =
    parameter("profile".?) { qsProfile =>
      optionalHeaderValueByName("Prefer") { preferHeader =>
        val name = qsProfile
          .orElse(preferHeader.flatMap(parsePreferProfile))
          .getOrElse(DefaultProfile)
        inner(name)
      }
    }

  private def parsePreferProfile(header: String): Option[String] =
    header.split(';').iterator.map(_.trim).collectFirst {
      case s"deidentify-profile=$name" => name.stripPrefix("\"").stripSuffix("\"")
    }

  // ---------- discovery resources

  private lazy val operationDefinition: OperationDefinition = {
    val op = new OperationDefinition()
    op.setId("deidentify")
    op.setUrl("https://ume.de/fhir/OperationDefinition/deidentify")
    op.setName("Deidentify")
    op.setTitle("DeidentiFHIR $deidentify operation")
    op.setStatus(org.hl7.fhir.r4.model.Enumerations.PublicationStatus.ACTIVE)
    op.setKind(OperationDefinition.OperationKind.OPERATION)
    op.setCode("deidentify")
    op.setAffectsState(false)
    op.setSystem(true)
    op.setType(true)
    op.setInstance(false)
    op.setDescription(
      "Whitelist-based de-identification of any FHIR R4 Resource (including " +
      "Bundles). The transformation is configured server-side; the input " +
      "resource is returned with non-whitelisted fields removed and " +
      "whitelisted fields optionally transformed (hashed, redacted, shifted, " +
      "etc.). Profile selection via ?profile= or Prefer: deidentify-profile=<name>."
    )
    op.addParameter()
      .setName("resource").setUse(OperationDefinition.OperationParameterUse.IN)
      .setMin(1).setMax("1").setType("Resource")
    op.addParameter()
      .setName("return").setUse(OperationDefinition.OperationParameterUse.OUT)
      .setMin(1).setMax("1").setType("Resource")
    op
  }

  private lazy val capabilityStatement: CapabilityStatement = {
    val cs = new CapabilityStatement()
    cs.setId("deidentifhir-service")
    cs.setStatus(org.hl7.fhir.r4.model.Enumerations.PublicationStatus.ACTIVE)
    cs.setKind(CapabilityStatement.CapabilityStatementKind.INSTANCE)
    cs.setSoftware(new CapabilityStatement.CapabilityStatementSoftwareComponent()
      .setName("DeidentiFHIR Service")
      .setVersion("0.2.11"))
    cs.setFhirVersion(org.hl7.fhir.r4.model.Enumerations.FHIRVersion._4_0_1)
    cs.setFormat(java.util.Arrays.asList(
      new org.hl7.fhir.r4.model.CodeType("application/fhir+json"),
      new org.hl7.fhir.r4.model.CodeType("application/json")
    ))
    val rest = cs.addRest()
    rest.setMode(CapabilityStatement.RestfulCapabilityMode.SERVER)
    rest.addOperation()
      .setName("deidentify")
      .setDefinition("https://ume.de/fhir/OperationDefinition/deidentify")
    cs
  }

  private def jsonResponse(r: Resource): HttpResponse = HttpResponse(
    StatusCodes.OK,
    entity = HttpEntity(FhirJson, fhirContext.newJsonParser().encodeResourceToString(r))
  )

  private def operationOutcome(status: StatusCode, code: String, diag: String): HttpResponse = {
    val oo = new org.hl7.fhir.r4.model.OperationOutcome()
    val issue = oo.addIssue()
    issue.setSeverity(org.hl7.fhir.r4.model.OperationOutcome.IssueSeverity.ERROR)
    issue.setCode(org.hl7.fhir.r4.model.OperationOutcome.IssueType.fromCode(code))
    issue.setDiagnostics(diag)
    HttpResponse(status, entity = HttpEntity(FhirJson, fhirContext.newJsonParser().encodeResourceToString(oo)))
  }

  // The lazy PII contract makes "ready" trivially true: the service can
  // accept traffic even before the model is loaded; the load happens on the
  // first matching redact call. We emit 200 as long as the registry built.
  private def readyResponse: HttpResponse =
    HttpResponse(StatusCodes.OK, entity = HttpEntity(ContentTypes.`text/plain(UTF-8)`, "ready"))
}
