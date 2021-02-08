package de.stereotypez.deidentifhir.codegen

import com.typesafe.scalalogging.LazyLogging
import de.stereotypez.deidentifhir.{DeidentifhirUtils, FhirProperty}
import de.stereotypez.deidentifhir.DeidentifhirUtils._
import org.hl7.fhir.r4.model.{Base, BaseResource, Element}
import org.reflections.Reflections

import java.io.{FileOutputStream, OutputStream, PrintWriter}
import java.lang.reflect.{Modifier, ParameterizedType, Type}
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.{Success, Try, Using}

object ProfileGenerator extends App with LazyLogging {

  //generate(System.out)

  println(s"Full profile was written to ${writeFile("src", "main", "resources", "profiles", "full.profile")(generate(_))}")
  println(s"Preliminary baseline profile was written to ${writeFile("src", "main", "resources", "profiles", "baseline.profile")(generate(_, baseline = true))}")

  def writeFile(elements: String*)(generator: PrintWriter => Unit): Path = {
    val path = Paths.get(elements.head, elements.tail:_*)
    path.toFile.delete()
    path.toFile.getParentFile.mkdirs()
    val file = Files.createFile(path).toFile
    Using.resource(new PrintWriter(new FileOutputStream(file)))(generator)
    path
  }

  def generate(printer: PrintWriter, baseline: Boolean = false): Unit = {
    new Reflections("org.hl7.fhir.r4.model").getSubTypesOf(classOf[BaseResource]).asScala.toSeq
      .filterNot(_.isInterface)
      .filterNot(c => Modifier.isAbstract(c.getModifiers))
      .sortBy(_.getDeclaredConstructor().newInstance().fhirType())
      .foreach { r =>
        val fhirType = r.getDeclaredConstructor().newInstance().fhirType()
        val paths = children(r, Seq(fhirType))
          .filterNot { p =>
            baseline &&
            {
              val tokens = p.split("\\.").toSeq
              tokens.exists(token => Seq("meta","extension","address","telecom", "photo").contains(token)) ||
              Seq("text", "display", "description", "name", "title", "comment").contains(tokens.last) ||
              tokens.endsWith(Seq("identifier","value")) ||
              tokens.containsSlice(Seq("Patient","name"))
            }
          }

        printer.println(s"\n# [$fhirType]")
        printer.println(paths.sorted.mkString("\n"))
      }
  }



  def children[T <: Base](clazz: Class[T], path: Seq[String]): Seq[String] = {

    logger.info(s"${clazz.getSimpleName} [${path.mkString(".")}]")

    clazz match {
      case c if c.isInterface => Seq()
      case c if Modifier.isAbstract(c.getModifiers) => Seq()
      case c =>
        val r = c.getDeclaredConstructor().newInstance()
        val cc = DeidentifhirUtils.getChildren(r)
        val ccc: Seq[String] = cc
          .filterNot(child => Seq("id", "extension").contains(child.field.getName))
          .filterNot(child => fhirCircuitBreak(child.field.getName, path))
          .collect {
            case child if classOf[java.util.List[_]].isAssignableFrom(child.field.getType) =>
              Try{
                val targs = child.field.getGenericType.asInstanceOf[ParameterizedType].getActualTypeArguments
                (targs.head.asInstanceOf[Class[_]], child.field.getName)
              }
            case child => child.field.getType
              Try(child.field.getType, child.field.getName)
          }
          .collect {
            case Success((c, n)) if classOf[Base].isAssignableFrom(c) =>
              children(c.asInstanceOf[Class[Base]], path :+ n)
          }
          .flatten
        path.mkString(".") +: ccc
    }

  }

}
