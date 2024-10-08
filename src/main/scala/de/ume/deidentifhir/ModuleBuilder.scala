package de.ume.deidentifhir

import com.typesafe.config.{Config, ConfigObject}
import de.ume.deidentifhir.Deidentifhir.DeidentifhirHandler
import de.ume.deidentifhir.ModuleBuilder.determineBaseProfilePaths
import de.ume.deidentifhir.util.DeidentifhirUtils.determinePattern
import de.ume.deidentifhir.util.Handlers

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag
import scala.util.matching.Regex

object ModuleBuilder {
  def apply(config: Config): ModuleBuilder = {
    new ModuleBuilder(config, new Registry())
  }

  def apply(config: Config, registry: Registry): ModuleBuilder = {
    new ModuleBuilder(config, registry)
  }

  // TODO add a check to make sure that the same base path is not contained twice!!!
  def determineBaseProfilePaths(conf: Config) = {
    val base = conf.getList("base")
    val buffer = new ListBuffer[String]
    base.forEach(path => {
      buffer.addOne(path.unwrapped().toString)
    })
    buffer.toSeq
  }
}

class ModuleBuilder(pattern: FhirPath, fullProfilePaths: Seq[String]) {

  private val typeHandlers = new mutable.HashMap[Class[_], Option[Seq[DeidentifhirHandler[Any]]]]()
  private val pathHandlers = new mutable.HashMap[String, Option[Seq[DeidentifhirHandler[Any]]]]()

  def this(conf: Config, registry: Registry) = {
    this(determinePattern(conf.getString("pattern")), determineBaseProfilePaths(conf))

    val base = conf.getList("base")
    base.forEach(path => {
      registerPlainPath(path.unwrapped().toString, Handlers.keepHandler)
    })

    def registerHandlersInConfigSection(section: String, handlerRegistrationLogic: (String, DeidentifhirHandler[_]) => Unit): Unit = {

      // check if this module section is present at all
      if(!conf.hasPath(section)) {
        return
      }

      conf.getConfig(section).root().entrySet().forEach( entry => {
        val path = entry.getKey
        val configObject = entry.getValue.asInstanceOf[ConfigObject]
        val handlerName = configObject.toConfig.getString("handler")
        val maybeHandler = registry.getHandler(handlerName)
        maybeHandler match {
          case Some(handler) => handlerRegistrationLogic(path, handler)
          case None          => throw new IllegalStateException(s"Cannot resolve requested handler '$handlerName'!")
        }
      })
    }

    registerHandlersInConfigSection("paths", (path: String, handler: DeidentifhirHandler[_]) => {
      register(path, Some(handler))
    })

    registerHandlersInConfigSection("types", (typeString: String, handler: DeidentifhirHandler[_]) => {
      register(handler)(ClassTag(Class.forName(s"org.hl7.fhir.r4.model.$typeString")))
    })
  }

  def register[T <: Any](handler: DeidentifhirHandler[T])(implicit t:ClassTag[T]): ModuleBuilder = {
    typeHandlers(t.runtimeClass) = typeHandlers.get(t.runtimeClass)
      .flatMap(_.map(handlers => handlers :+ handler.asInstanceOf[DeidentifhirHandler[Any]]))
      .orElse(Option(Seq(handler.asInstanceOf[DeidentifhirHandler[Any]])))
    this
  }

  private def registerPlainPath[T <: Any](path: String, handler: Option[DeidentifhirHandler[T]]): ModuleBuilder = {

    // check that the path is valid, to catch possible config errors early
    if(fullProfilePaths.contains(path)){
      pathHandlers(path) = pathHandlers.get(path)
        .flatMap(_.map(handlers => handler.map(newhandler => handlers :+ newhandler.asInstanceOf[DeidentifhirHandler[Any]]).getOrElse(handlers)))
        .orElse(handler.map(_.asInstanceOf[DeidentifhirHandler[Any]]).map(Seq(_)))
      this
    } else {
      throw new Exception(s"trying to register a handler to the unspecified FHIR path $path!")
    }
  }

  /**
   * Add path as Regex.
   * For later efficiency when running the deidentification, we perform a full match against our
   * (hopefully) complete set of available paths and add them as plain strings.
   */
  def register[T <: Any](path: Regex, handler: Option[DeidentifhirHandler[T]]): ModuleBuilder = {
    fullProfilePaths
      .filter(path.matches)
      .foreach(registerPlainPath(_, handler))
    this
  }

  /**
   * Add path either plain or in a simplified glob-like * syntax.
   */
  def register[T <: Any](path: String, handler: Option[DeidentifhirHandler[T]]): ModuleBuilder = {

    path match {
      // add plain path
      case _ if !path.contains('*') =>
        registerPlainPath(path, handler)

      // Compiles some trivial * syntax into regex.
      case _ =>
        def toPattern(elements: Seq[String]) = {
          elements
            .zipWithIndex
            .map { case (elem, i) =>
              (elem, i) match {
                case ("*", 0) => ".*"
                case (s, _) => s.replaceAll("\\*", "[^\\.]*")
              }
            }
            .mkString("\\.")
        }

        val elements = path.split("\\.").toSeq

        val pattern = elements match {
          case e if e.length == 1 =>
            e.head match {
              case "*" => ".*"
              case s => s
            }
          case e if e.length > 1 && e.head == "*" && e.last == "*" =>
            "^(|.*\\.)" + toPattern(e.drop(1).dropRight(1)) + "(|\\..*)$"
          case e if e.head == "*" =>
            "^(|.*\\.)" + toPattern(e.drop(1)) + "$"
          case e if e.last == "*" =>
            "^" + toPattern(e.dropRight(1)) + "(|\\..*)$"
          case e =>
            "^" + toPattern(e) + "$"
        }

        register(new Regex(pattern), handler)
    }
  }

  def build(): Module = {
    Module(pattern, pathHandlers.toMap, typeHandlers.toMap)
  }
}
