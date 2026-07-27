package de.ume.deidentifhir.service

import org.yaml.snakeyaml.Yaml

import java.io.{InputStream, InputStreamReader}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

/** Top-level service configuration parsed from YAML.
 *
 * Schema (admin-facing):
 * {{{
 *   service:
 *     host: 0.0.0.0
 *     port: 8080
 *
 *   # Optional. Only consulted if at least one handler in any profile names
 *   # type: redact-pii. Otherwise the openai/privacy-filter is never even
 *   # instantiated, never mind loaded.
 *   pii-openai:
 *     cache-dir: /var/lib/deidentifhir-pii
 *     min-score: 0.5
 *     prewarm: false
 *
 *   profiles:
 *     default:
 *       paths:
 *         - { path: "Patient.id",                handler: passthrough }
 *         - { path: "Patient.name.family",       handler: redact-pii }
 *         - { path: "Patient.birthDate",         handler: date-shift, days: -7 }
 *         - { path: "Observation.note.text",     handler: redact-pii }
 * }}}
 *
 * Each path entry is `{path, handler, ...handler-specific args}`. The args
 * map is forwarded to the handler factory uninterpreted, so adding a new
 * handler type doesn't require changing the parser.
 */
final case class ServiceConfig(
    service: ServerConfig,
    piiOpenai: Option[PiiOpenaiConfig],
    profiles: Map[String, ProfileConfig]
) {
  /** True iff at least one path in any profile targets the redact-pii handler.
   * Drives lazy construction of the openai/privacy-filter wrapper. */
  def usesPiiOpenai: Boolean =
    profiles.values.exists(_.paths.exists(_.handler == "redact-pii"))
}

final case class ServerConfig(host: String = "0.0.0.0", port: Int = 8080)

final case class PiiOpenaiConfig(
    cacheDir: Option[String] = None,
    minScore: Double = 0.5,
    prewarm: Boolean = false
)

final case class ProfileConfig(paths: Seq[PathSpec])

/** A single FHIR path → handler binding from the YAML config. The `args` map
 * holds any handler-specific arguments (e.g. `days: -7` for `date-shift`),
 * forwarded as-is to the handler factory. */
final case class PathSpec(path: String, handler: String, args: Map[String, Any])

object ServiceConfig {

  def load(path: Path): ServiceConfig = {
    val in = Files.newInputStream(path)
    try parseStream(in) finally in.close()
  }

  def loadResource(name: String): ServiceConfig = {
    val in = Option(getClass.getClassLoader.getResourceAsStream(name))
      .getOrElse(sys.error(s"resource not found: $name"))
    try parseStream(in) finally in.close()
  }

  def parse(yaml: String): ServiceConfig =
    parseStream(new java.io.ByteArrayInputStream(yaml.getBytes(StandardCharsets.UTF_8)))

  private def parseStream(in: InputStream): ServiceConfig = {
    val raw = new Yaml().load[Any](new InputStreamReader(in, StandardCharsets.UTF_8))
    val root = asMap(raw, "<root>")
    ServiceConfig(
      service = parseServer(root.get("service")),
      piiOpenai = root.get("pii-openai").map(parsePiiOpenai),
      profiles = parseProfiles(root.getOrElse("profiles", sys.error("config: missing 'profiles'")))
    )
  }

  private def parseServer(node: Option[Any]): ServerConfig = node match {
    case None => ServerConfig()
    case Some(v) =>
      val m = asMap(v, "service")
      ServerConfig(
        host = m.get("host").map(asString(_, "service.host")).getOrElse("0.0.0.0"),
        port = m.get("port").map(asInt(_, "service.port")).getOrElse(8080)
      )
  }

  private def parsePiiOpenai(node: Any): PiiOpenaiConfig = {
    val m = asMap(node, "pii-openai")
    PiiOpenaiConfig(
      cacheDir = m.get("cache-dir").map(asString(_, "pii-openai.cache-dir")),
      minScore = m.get("min-score").map(asDouble(_, "pii-openai.min-score")).getOrElse(0.5),
      prewarm = m.get("prewarm").map(asBool(_, "pii-openai.prewarm")).getOrElse(false)
    )
  }

  private def parseProfiles(node: Any): Map[String, ProfileConfig] = {
    val m = asMap(node, "profiles")
    if (m.isEmpty) sys.error("config: 'profiles' must contain at least one entry")
    m.map { case (name, body) =>
      val pm = asMap(body, s"profiles.$name")
      val pathsRaw = pm.getOrElse("paths", sys.error(s"config: profile '$name' missing 'paths'"))
      val paths = asList(pathsRaw, s"profiles.$name.paths").zipWithIndex.map { case (entry, i) =>
        parsePathSpec(entry, s"profiles.$name.paths[$i]")
      }
      name -> ProfileConfig(paths)
    }
  }

  private def parsePathSpec(node: Any, ctx: String): PathSpec = {
    val m = asMap(node, ctx)
    val path = m.get("path").map(asString(_, s"$ctx.path"))
      .getOrElse(sys.error(s"config: $ctx missing 'path'"))
    val handler = m.get("handler").map(asString(_, s"$ctx.handler"))
      .getOrElse(sys.error(s"config: $ctx missing 'handler'"))
    val args = (m - "path" - "handler").asInstanceOf[Map[String, Any]]
    PathSpec(path, handler, args)
  }

  // ---------- snakeyaml→scala adapters

  private def asMap(v: Any, ctx: String): Map[String, Any] = v match {
    case null => sys.error(s"config: $ctx is null")
    case m: java.util.Map[_, _] =>
      m.asScala.toMap.map { case (k, vv) => k.toString -> (vv: Any) }
    case other => sys.error(s"config: $ctx expected mapping, got ${other.getClass.getSimpleName}")
  }

  private def asList(v: Any, ctx: String): Seq[Any] = v match {
    case null => sys.error(s"config: $ctx is null")
    case l: java.util.List[_] => l.asScala.toSeq.map(x => x: Any)
    case other => sys.error(s"config: $ctx expected list, got ${other.getClass.getSimpleName}")
  }

  private def asString(v: Any, ctx: String): String = v match {
    case s: String => s
    case n: Number => n.toString
    case b: java.lang.Boolean => b.toString
    case other => sys.error(s"config: $ctx expected string, got ${other.getClass.getSimpleName}")
  }

  private def asInt(v: Any, ctx: String): Int = v match {
    case n: java.lang.Integer => n.intValue
    case n: java.lang.Long    => n.intValue
    case n: java.lang.Number  => n.intValue
    case s: String            => try s.toInt catch { case _: Throwable => sys.error(s"config: $ctx not a number: $s") }
    case other => sys.error(s"config: $ctx expected int, got ${other.getClass.getSimpleName}")
  }

  private def asDouble(v: Any, ctx: String): Double = v match {
    case n: java.lang.Number => n.doubleValue
    case s: String           => try s.toDouble catch { case _: Throwable => sys.error(s"config: $ctx not a number: $s") }
    case other => sys.error(s"config: $ctx expected number, got ${other.getClass.getSimpleName}")
  }

  private def asBool(v: Any, ctx: String): Boolean = v match {
    case b: java.lang.Boolean => b.booleanValue
    case s: String => s.toLowerCase match {
      case "true" | "yes" | "on" => true
      case "false" | "no" | "off" => false
      case _ => sys.error(s"config: $ctx not a boolean: $s")
    }
    case other => sys.error(s"config: $ctx expected boolean, got ${other.getClass.getSimpleName}")
  }
}
