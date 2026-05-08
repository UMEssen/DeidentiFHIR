package de.ume.deidentifhir.service

import com.typesafe.scalalogging.LazyLogging
import org.apache.pekko.actor.typed.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.Http.ServerBinding

import java.nio.file.{Path, Paths}
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Try

/** Entry point for `deidentifhir-service-http`.
 *
 * Loads service config, builds the [[ProfileRegistry]], binds the pekko-http
 * routes, and registers a JVM shutdown hook for graceful drain.
 *
 * Config-source resolution (first hit wins):
 *   1. CLI arg: `java -jar … <path-to-yaml>`
 *   2. `DEIDENTIFHIR_CONFIG` env var
 *   3. `application.yaml` in the working directory
 *   4. `application.yaml` packaged on the classpath (a small example default)
 */
object Main extends LazyLogging {

  def main(args: Array[String]): Unit = {
    val configPath = resolveConfigPath(args)
    val config = configPath match {
      case Some(p) =>
        logger.info(s"Loading config from $p")
        ServiceConfig.load(p)
      case None =>
        logger.info("Loading bundled default config (application.yaml on classpath)")
        ServiceConfig.loadResource("application.yaml")
    }

    val registry = ProfileRegistry.build(config)
    logger.info(s"Built ${registry.profileNames.size} profile(s): ${registry.profileNames.toSeq.sorted.mkString(", ")}")

    implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "deidentifhir-service")

    val routes = new Routes(registry).routes
    val bindingF: Future[ServerBinding] = Http().newServerAt(config.service.host, config.service.port).bind(routes)

    val binding = Await.result(bindingF, 30.seconds)
    logger.info(s"Listening on ${config.service.host}:${config.service.port}")
    logger.info(s"  POST /fhir/$$deidentify           — system-level")
    logger.info(s"  POST /fhir/{Type}/$$deidentify    — type-level")
    logger.info(s"  GET  /fhir/metadata               — capability statement")
    logger.info(s"  GET  /fhir/OperationDefinition/deidentify")
    logger.info(s"  GET  /healthz, /readyz")

    sys.addShutdownHook {
      logger.info("Shutting down …")
      Try(Await.result(binding.unbind(), 10.seconds))
      Try(registry.close())
      Try(Await.result(system.whenTerminated, 10.seconds))
      system.terminate()
    }

    Await.result(system.whenTerminated, Duration.Inf)
  }

  /** First-hit-wins config-source resolution. Returns `None` to mean "use the
   * classpath default". */
  private def resolveConfigPath(args: Array[String]): Option[Path] = {
    val fromArg = args.headOption
    val fromEnv = sys.env.get("DEIDENTIFHIR_CONFIG")
    val fromCwd = Some("application.yaml").filter(p => java.nio.file.Files.exists(Paths.get(p)))
    fromArg.orElse(fromEnv).orElse(fromCwd).map(Paths.get(_))
  }
}
