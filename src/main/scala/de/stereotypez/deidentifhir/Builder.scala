package de.stereotypez.deidentifhir

import com.typesafe.scalalogging.LazyLogging
import de.stereotypez.deidentifhir.Deidentifhir.DeidentifhirHandler

import java.net.URL
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.matching.Regex
import scala.util.{Failure, Success, Using}

/**
 * @author ${user.name}
 */
object Builder {
  def apply() = new Builder()
  def apply(url: URL): Builder = new Builder(url)
}

class Builder() extends LazyLogging {

  lazy private val fullProfilePaths: Seq[String] = Using(scala.io.Source.fromURL(Profile.Full)) { source =>
    source.getLines()
      .map(_.trim)
      .filterNot(_.isBlank)
      .filterNot(_.startsWith("#"))
  } match {
    case Success(value) => value.toSeq
    case Failure(ex) => throw ex
  }

  private val typeHandlers = new mutable.HashMap[Class[_], Option[Seq[DeidentifhirHandler[Any]]]]()
  private val pathHandlers = new mutable.HashMap[String, Option[Seq[DeidentifhirHandler[Any]]]]()

  def this(url: URL) {
    this()
    Using(scala.io.Source.fromURL(url)) { source =>
      pathHandlers.addAll(
        source.getLines()
          .map(_.trim)
          .filterNot(_.isBlank)
          .filterNot(_.startsWith("#"))
          .map((_, None))
      )
      this
    } match {
      case Success(value) => value
      case Failure(ex) => throw ex
    }
  }

  def register[T <: Any](handler: DeidentifhirHandler[T])(implicit t:ClassTag[T]): Builder = {
    typeHandlers(t.runtimeClass) = typeHandlers.get(t.runtimeClass)
      .flatMap(_.map(handlers => handlers :+ handler.asInstanceOf[DeidentifhirHandler[Any]]))
      .orElse(Option(Seq(handler.asInstanceOf[DeidentifhirHandler[Any]])))
    this
  }

  private def registerPlainPath[T <: Any](path: String, handler: Option[DeidentifhirHandler[T]]): Builder = {
    pathHandlers(path) = pathHandlers.get(path)
      .flatMap(_.map(handlers => handler.map(newhandler => handlers :+ newhandler.asInstanceOf[DeidentifhirHandler[Any]]).getOrElse(handlers)))
      .orElse(handler.map(_.asInstanceOf[DeidentifhirHandler[Any]]).map(Seq(_)))
    this
  }

  /**
   * Add path as Regex.
   * For later efficiency when running the deidentification, we perform a full match against our
   * (hopefully) complete set of available paths and add them as plain strings.
   */
  def register[T <: Any](path: Regex, handler: Option[DeidentifhirHandler[T]]): Builder = {
    fullProfilePaths
      .filter(path.matches)
      .foreach(registerPlainPath(_, handler))
    this
  }

  /**
   * Add path either plain or in a simplified glob-like * syntax.
   */
  def register[T <: Any](path: String, handler: Option[DeidentifhirHandler[T]]): Builder = {

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

  def build(): Deidentifhir = {
    new Deidentifhir(pathHandlers.toMap, typeHandlers.toMap)
  }

}