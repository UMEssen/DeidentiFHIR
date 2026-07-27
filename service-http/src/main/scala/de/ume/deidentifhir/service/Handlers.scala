package de.ume.deidentifhir.service

import de.ume.deidentifhir.Deidentifhir.DeidentifhirHandler
import de.ume.deidentifhir.ModuleBuilder
import de.ume.deidentifhir.pii.openai.{PrivacyFilter, PrivacyFilterRedactor}
import de.ume.deidentifhir.util.Helpers
import de.ume.deidentifhir.util.{Handlers => UmeHandlers}
import org.hl7.fhir.r4.model.{Base, BaseDateTimeType, PrimitiveType}

import java.util.Calendar

/** Registry of handler factories. Each factory understands one `handler:` value
 * from the YAML config and registers an appropriate [[DeidentifhirHandler]] on
 * the [[ModuleBuilder]] for a given path.
 *
 * The factory contract is intentionally narrow: take a [[PathSpec]] (which
 * carries the args map verbatim from the YAML) plus access to shared deps
 * (currently just the optional [[PrivacyFilter]]), and register on the
 * builder.
 *
 * Adding a new handler type: implement a [[HandlerFactory]] case object,
 * append it to [[Handlers.byName]]. No parser changes needed — args are
 * pulled out of the args map as needed.
 *
 * Where reasonable we delegate to UMEssen's built-in handlers in
 * [[de.ume.deidentifhir.util.Handlers]] (e.g. `passthrough` is just
 * `keepHandler`); custom handlers are inline.
 */
private[service] trait HandlerFactory {
  def name: String

  /** Register the handler at `spec.path` on `builder`. Throws if `spec.args`
   * is missing required keys or this handler needs deps that weren't passed. */
  def register(builder: ModuleBuilder, spec: PathSpec, deps: HandlerDeps): ModuleBuilder
}

/** Resources a handler factory may need. `privacyFilter` is `Some` iff the
 * config references the redact-pii handler at least once — otherwise the
 * filter is never even constructed. */
private[service] final case class HandlerDeps(privacyFilter: Option[PrivacyFilter])

private[service] object Handlers {

  /** Identity handler — keeps a path's value unchanged. Delegates to UMEssen's
   * built-in `keepHandler`, which is the canonical "preserve" sentinel
   * (`None` Option). */
  case object Passthrough extends HandlerFactory {
    val name = "passthrough"
    def register(builder: ModuleBuilder, spec: PathSpec, deps: HandlerDeps): ModuleBuilder = {
      builder.register(spec.path, UmeHandlers.keepHandler)
    }
  }

  /** SHA-256 the value's string representation. Useful for IDs, identifiers,
   * URIs, etc. — anything where you want a stable pseudo-id but not the
   * original. Targets any string-like primitive (StringType, IdType, UriType,
   * CodeType, …) by matching on the parametric parent. */
  case object Sha256 extends HandlerFactory {
    val name = "sha256"
    def register(builder: ModuleBuilder, spec: PathSpec, deps: HandlerDeps): ModuleBuilder = {
      val h: DeidentifhirHandler[Base] =
        (_: Seq[String], v: Base, _: Seq[Base], _: Map[String, String]) => v match {
          case p: PrimitiveType[_] if p.getValue.isInstanceOf[String] =>
            val raw = p.getValueAsString
            if (raw == null) p else { p.setValueAsString(Helpers.sha256(raw)); p }
          case other => other
        }
      builder.register(spec.path, Some(h))
    }
  }

  /** Replace the value with a literal string. `args.value` is the replacement;
   * defaults to `[REDACTED]`. Delegates to UMEssen's `stringReplacementHandler`
   * for StringType paths; falls back to a typed handler for non-StringType
   * primitive-string paths (IdType, UriType, …). */
  case object Replace extends HandlerFactory {
    val name = "replace"
    def register(builder: ModuleBuilder, spec: PathSpec, deps: HandlerDeps): ModuleBuilder = {
      val replacement = spec.args.get("value").map(_.toString).getOrElse("[REDACTED]")
      val h: DeidentifhirHandler[Base] =
        (_: Seq[String], v: Base, _: Seq[Base], _: Map[String, String]) => v match {
          case p: PrimitiveType[_] if p.getValue.isInstanceOf[String] =>
            p.setValueAsString(replacement); p
          case other => other
        }
      builder.register(spec.path, Some(h))
    }
  }

  /** Constant-shift any [[BaseDateTimeType]] by `args.days` days. Defaults to
   * 0 (i.e. no-op). Negative values move dates earlier; positive later.
   *
   * Distinct from UMEssen's `Handlers.shiftDateHandler`, which expects a
   * `ShiftDateProvider` and a per-patient identifier in the staticContext.
   * This handler is for the simpler case of a literal day offset declared
   * once in YAML. */
  case object DateShift extends HandlerFactory {
    val name = "date-shift"
    def register(builder: ModuleBuilder, spec: PathSpec, deps: HandlerDeps): ModuleBuilder = {
      val days = spec.args.get("days").map {
        case n: java.lang.Number => n.intValue
        case s: String           => s.toInt
        case other               => sys.error(s"date-shift.days: expected int, got $other")
      }.getOrElse(0)
      val h: DeidentifhirHandler[Base] =
        (_: Seq[String], v: Base, _: Seq[Base], _: Map[String, String]) => v match {
          case d: BaseDateTimeType =>
            val shifted = d.copy().asInstanceOf[BaseDateTimeType]
            shifted.add(Calendar.DAY_OF_MONTH, days)
            shifted
          case other => other
        }
      builder.register(spec.path, Some(h))
    }
  }

  /** Run the value through the openai/privacy-filter PII redactor. Requires
   * `deps.privacyFilter` to be `Some` — i.e. that the service-level config
   * actually had a `pii-openai` block (and that the redact-pii handler was
   * referenced anywhere, which is the gate that constructs the filter at all). */
  case object RedactPii extends HandlerFactory {
    val name = "redact-pii"
    def register(builder: ModuleBuilder, spec: PathSpec, deps: HandlerDeps): ModuleBuilder = {
      val pf = deps.privacyFilter.getOrElse(
        sys.error("redact-pii handler used but PrivacyFilter wasn't constructed " +
          "(internal bug: ServiceConfig.usesPiiOpenai must be true here)")
      )
      val redactor = new PrivacyFilterRedactor(pf)
      builder.register(spec.path, Some(redactor.asStringTypeHandler))
    }
  }

  val all: Seq[HandlerFactory] = Seq(Passthrough, Sha256, Replace, DateShift, RedactPii)

  val byName: Map[String, HandlerFactory] = all.map(f => f.name -> f).toMap

  /** Validate that every handler name referenced by the config exists in the
   * registry. Called once at startup so misconfiguration fails before HTTP
   * traffic, not on the first request. */
  def validate(config: ServiceConfig): Unit = {
    val unknown = for {
      (profileName, profile) <- config.profiles.toSeq
      spec <- profile.paths
      if !byName.contains(spec.handler)
    } yield s"profile '$profileName': unknown handler '${spec.handler}' at path '${spec.path}'"
    if (unknown.nonEmpty) sys.error("config:\n  " + unknown.mkString("\n  "))
  }
}
