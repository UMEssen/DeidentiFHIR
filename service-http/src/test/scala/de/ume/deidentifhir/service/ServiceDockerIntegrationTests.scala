package de.ume.deidentifhir.service

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import org.testcontainers.containers.GenericContainer
import org.testcontainers.containers.wait.strategy.Wait
import org.testcontainers.images.builder.ImageFromDockerfile

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.{Path, Paths}
import java.time.Duration

/** Dockerized end-to-end test for the service-http module.
 *
 * Builds the actual Docker image from `service-http/Dockerfile` against the
 * repo root as build context, starts it via Testcontainers, and exercises
 * the FHIR `$deidentify` operation, discovery resources, and probes through
 * a real HTTP socket. This is the canonical proof that the ship artifact
 * works — same Dockerfile, same packaging, same runtime path a deployed
 * instance would take.
 *
 * Slow (image build is ~1-3 min on first run, cached afterward) and
 * requires Docker on the host, so gated behind the system property
 * `deidentifhir.service.docker`. Without the flag, the suite never touches
 * Docker — the container isn't constructed, no API probe, all tests
 * cancel via `assume`.
 *
 * Run:
 *   mvn -pl service-http test -Ddeidentifhir.service.docker=1
 *
 * '''Docker Desktop with Enhanced Container Isolation (ECI):''' if you
 * see "Could not find a valid Docker environment" with HTTP 400 responses
 * from the Docker daemon, Docker Desktop's ECI is blocking testcontainers'
 * Docker-API access. Disable ECI in Docker Desktop → Settings → General,
 * restart Docker, retry. `docker build` and `docker run` keep working
 * under ECI because they go through the privileged Docker CLI; testcontainers'
 * raw `/info` and `/auth` calls don't. Linux Docker daemons and CI runners
 * are unaffected.
 */
class ServiceDockerIntegrationTests extends AnyFunSuite with BeforeAndAfterAll {

  private val Enabled =
    sys.props.get("deidentifhir.service.docker").contains("1") ||
      sys.env.get("DEIDENTIFHIR_SERVICE_DOCKER").contains("1")

  // Locate the repo root from wherever the test is launched. mvn runs in
  // service-http/, ScalaTest's standalone runner usually runs from the
  // repo root. Walk up until we find a dir with both pom.xml and a
  // service-http/ child — the parent reactor's directory.
  private lazy val repoRoot: Path = {
    var p = Paths.get("").toAbsolutePath
    while (p != null && !(p.resolve("pom.xml").toFile.exists() &&
                          p.resolve("service-http").toFile.isDirectory)) {
      p = p.getParent
    }
    if (p == null) sys.error("can't locate repo root (no parent has pom.xml + service-http/)")
    p
  }

  // Manual container lifecycle so we can completely skip Docker contact
  // when the flag isn't set. ForAllTestContainer would start the container
  // at suite-construction time, which probes Docker before withFixture's
  // assume() can gate it.
  private var container: GenericContainer[_] = _

  override def beforeAll(): Unit = {
    if (!Enabled) return
    val image = new ImageFromDockerfile("deidentifhir-service-test", false)
      .withFileFromPath(".", repoRoot)
      .withDockerfilePath("service-http/Dockerfile")

    val c = new GenericContainer(image)
    c.addExposedPort(Integer.valueOf(8080))
    c.setWaitStrategy(Wait.forHttp("/healthz").forStatusCode(200)
      .withStartupTimeout(Duration.ofMinutes(5)))
    c.start()
    container = c
  }

  override def afterAll(): Unit = {
    if (container != null) container.stop()
  }

  override def withFixture(test: NoArgTest): org.scalatest.Outcome = {
    assume(Enabled, "set -Ddeidentifhir.service.docker=1 to run")
    super.withFixture(test)
  }

  private def baseUrl: String =
    s"http://${container.getHost}:${container.getMappedPort(8080)}"

  private val http: HttpClient = HttpClient.newBuilder()
    .connectTimeout(Duration.ofSeconds(5))
    .build()

  private def get(path: String): HttpResponse[String] =
    http.send(
      HttpRequest.newBuilder().uri(URI.create(baseUrl + path)).GET().build(),
      HttpResponse.BodyHandlers.ofString()
    )

  private def postFhir(path: String, body: String): HttpResponse[String] =
    http.send(
      HttpRequest.newBuilder()
        .uri(URI.create(baseUrl + path))
        .header("Content-Type", "application/fhir+json")
        .POST(HttpRequest.BodyPublishers.ofString(body))
        .build(),
      HttpResponse.BodyHandlers.ofString()
    )

  // ---------- tests

  test("healthz returns 200 ok") {
    val r = get("/healthz")
    assert(r.statusCode == 200)
    assert(r.body == "ok")
  }

  test("readyz returns 200") {
    assert(get("/readyz").statusCode == 200)
  }

  test("metadata returns a CapabilityStatement listing the deidentify operation") {
    val r = get("/fhir/metadata")
    assert(r.statusCode == 200)
    assert(r.body.contains("\"resourceType\":\"CapabilityStatement\""))
    assert(r.body.contains("\"deidentify\""))
  }

  test("OperationDefinition resource is discoverable") {
    val r = get("/fhir/OperationDefinition/deidentify")
    assert(r.statusCode == 200)
    assert(r.body.contains("\"resourceType\":\"OperationDefinition\""))
    assert(r.body.contains("\"code\":\"deidentify\""))
  }

  test("$deidentify on a Patient applies the bundled default profile") {
    val patient =
      """{"resourceType":"Patient","id":"123","gender":"female",""" +
      """"birthDate":"1980-05-15",""" +
      """"address":[{"postalCode":"45678","city":"Springfield"}]}"""
    val r = postFhir("/fhir/Patient/$deidentify", patient)
    assert(r.statusCode == 200, s"body: ${r.body}")
    val body = r.body
    // Default profile: id is sha256-hashed, birthDate -7 days, address fields
    // not whitelisted so they're removed.
    assert(!body.contains("\"id\":\"123\""), s"id should be hashed: $body")
    assert(body.contains("\"birthDate\":\"1980-05-08\""), s"birthDate should be -7 days: $body")
    assert(!body.contains("Springfield"), s"city should be removed: $body")
    assert(!body.contains("45678"),       s"postalCode should be removed: $body")
  }

  test("$deidentify on a Bundle dispatches per entry") {
    val bundle =
      """{"resourceType":"Bundle","type":"collection","entry":[""" +
      """{"resource":{"resourceType":"Patient","id":"abc","gender":"male","birthDate":"1972-09-30"}}""" +
      """]}"""
    val r = postFhir("/fhir/$deidentify", bundle)
    assert(r.statusCode == 200, s"body: ${r.body}")
    assert(r.body.contains("\"birthDate\":\"1972-09-23\""), s"bundle entry not deidentified: ${r.body}")
  }

  test("type-level $deidentify rejects body of the wrong resourceType") {
    val patient =
      """{"resourceType":"Patient","id":"x","gender":"female","birthDate":"1980-01-01"}"""
    val r = postFhir("/fhir/Observation/$deidentify", patient)
    assert(r.statusCode == 400, s"expected 400, got ${r.statusCode}: ${r.body}")
    assert(r.body.contains("OperationOutcome"))
    assert(r.body.contains("expected Observation"))
  }

  test("malformed JSON returns 400 OperationOutcome") {
    val r = postFhir("/fhir/$deidentify", "{not fhir")
    assert(r.statusCode == 400)
    assert(r.body.contains("OperationOutcome"))
  }

  test("unknown profile returns 404 OperationOutcome") {
    val patient = """{"resourceType":"Patient","id":"x"}"""
    val r = postFhir("/fhir/$deidentify?profile=does-not-exist", patient)
    assert(r.statusCode == 404)
    assert(r.body.contains("does-not-exist"))
  }
}
