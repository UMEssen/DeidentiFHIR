package de.ume.deidentifhir.service

import com.typesafe.scalalogging.LazyLogging
import de.ume.deidentifhir.pii.openai.{PrivacyFilter, PrivacyFilterConfig}
import de.ume.deidentifhir.{ApplyAlways, Deidentifhir, ModuleBuilder}

import java.nio.file.Paths

/** Holds one [[Deidentifhir]] instance per named profile and the (optional)
 * shared [[PrivacyFilter]] instance.
 *
 * Construction order:
 *   1. Validate the config against the handler registry.
 *   2. If any profile references redact-pii, construct a single [[PrivacyFilter]]
 *      (cheap — model loads on first call). Otherwise leave it `None`, so
 *      no DJL/ONNX classes are pulled in beyond the dep itself.
 *   3. Build a [[Deidentifhir]] for each profile by replaying handler
 *      registrations against a fresh [[ModuleBuilder]].
 *
 * Each profile maps to a single Module with `pattern = ApplyAlways()` — the
 * profile's handlers apply to every incoming resource. UMEssen's engine
 * supports more sophisticated routing via [[de.ume.deidentifhir.FhirPath]]
 * variants (per-profile, per-identifier-system); this service intentionally
 * keeps profile selection at the HTTP layer instead.
 *
 * Built [[Deidentifhir]] instances are immutable post-build, so they can be
 * shared across pekko-http worker threads without further synchronization.
 */
final class ProfileRegistry(
    config: ServiceConfig,
    val privacyFilter: Option[PrivacyFilter],
    deidentifiers: Map[String, Deidentifhir]
) extends AutoCloseable with LazyLogging {

  /** Look up a built deidentifier by profile name. */
  def get(profile: String): Option[Deidentifhir] = deidentifiers.get(profile)

  /** Names of all configured profiles. */
  def profileNames: Set[String] = deidentifiers.keySet

  override def close(): Unit = privacyFilter.foreach { pf =>
    try pf.close() catch { case e: Exception => logger.warn("PrivacyFilter close failed", e) }
  }
}

object ProfileRegistry extends LazyLogging {

  /** Materialize a [[ProfileRegistry]] from a parsed [[ServiceConfig]]. */
  def build(config: ServiceConfig): ProfileRegistry = {
    Handlers.validate(config)

    val pf: Option[PrivacyFilter] =
      if (config.usesPiiOpenai) Some(buildPrivacyFilter(config)) else None
    pf.foreach { f =>
      val warm = config.piiOpenai.exists(_.prewarm)
      if (warm) {
        logger.info("Pre-warming openai/privacy-filter on startup (prewarm=true)")
        f.prewarm()
      } else {
        logger.info("openai/privacy-filter will load on first redact call (prewarm=false)")
      }
    }

    val deps = HandlerDeps(privacyFilter = pf)
    val deidentifiers: Map[String, Deidentifhir] = config.profiles.map { case (name, profile) =>
      // The list of paths in the profile is also the module's "base" — every
      // path gets its handler registered explicitly, no implicit keepHandlers
      // beyond what the user declared with `passthrough`.
      val basePaths = profile.paths.map(_.path)
      val mb = new ModuleBuilder(ApplyAlways(), basePaths)
      profile.paths.foreach(spec => Handlers.byName(spec.handler).register(mb, spec, deps))
      val module = mb.build()
      logger.info(s"Built profile '$name' with ${profile.paths.size} path bindings")
      name -> new Deidentifhir(Seq(module))
    }

    new ProfileRegistry(config, pf, deidentifiers)
  }

  private def buildPrivacyFilter(config: ServiceConfig): PrivacyFilter = {
    val pioConfig = config.piiOpenai.getOrElse(throw new IllegalStateException(
      "config references redact-pii but no 'pii-openai' block is present — " +
        "add 'pii-openai: {}' to accept defaults, or remove the redact-pii handler"
    ))
    val pfBase = PrivacyFilterConfig.defaults
    val pfConfig = pfBase.copy(
      cacheDir = pioConfig.cacheDir.map(Paths.get(_)).getOrElse(pfBase.cacheDir),
      minScore = pioConfig.minScore
    )
    new PrivacyFilter(pfConfig)
  }
}
