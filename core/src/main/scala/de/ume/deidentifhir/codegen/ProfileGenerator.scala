package de.ume.deidentifhir.codegen

import com.typesafe.scalalogging.LazyLogging
import de.ume.deidentifhir.util.Hapi
import de.ume.deidentifhir.util.Hapi._
import org.hl7.fhir.r4.model.{Base, BaseResource, Enumeration, MoneyQuantity, Property, SimpleQuantity, Type}
import org.reflections.Reflections

import java.io.{FileOutputStream, PrintWriter}
import java.lang.reflect.{Modifier, ParameterizedType}
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.{Success, Try, Using}

object ProfileGenerator extends App with LazyLogging {

  // maps from a FHIR type code to the corresponding class
   val typeCodeToClassMapping = new Reflections("org.hl7.fhir.r4.model").getSubTypesOf(classOf[Type]).asScala.toSeq
    .filterNot(_.isInterface)
    .filterNot(c => Modifier.isAbstract(c.getModifiers))
    .map( clazz => (clazz.getDeclaredConstructor().newInstance().fhirType(), clazz))
    .map( value => value match {
      // since the fhirType doesn't always match the typeCode, we need special handling for these cases. Until I can find a more generic way...
      case (_, clazz) if clazz == classOf[SimpleQuantity] => ("SimpleQuantity", clazz)
      case (_, clazz) if clazz == classOf[MoneyQuantity]  => ("MoneyQuantity", clazz)
      case (_, clazz) if clazz == classOf[Enumeration[_]] => ("Enumeration", clazz)
      case v                                              => v
    })
    .toMap

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
        val paths = children(r, Seq(fhirType), None)
          .filterNot { p =>
            baseline &&
            {
              val tokens = p.split("\\.").toSeq
              tokens.exists(token => Seq("id", "meta","extension","address","telecom", "photo").contains(token)) ||
              Seq("text", "display", "description", "name", "title", "comment").contains(tokens.last) ||
              tokens.endsWith(Seq("identifier","value")) ||
              tokens.containsSlice(Seq("Patient","name"))
            }
          }

        printer.println(s"\n# [$fhirType]")
        printer.println(paths.sorted.mkString("\n"))
      }
  }

  def children[T <: Base](clazz: Class[T], path: Seq[String], property: Option[Property]): Seq[String] = {

    logger.info(s"${clazz.getSimpleName} [${path.mkString(".")}]")

    clazz match {
      case c if c.isInterface => Seq()
      // to include all elements that have a choice of type, we need to handle the abstract class Type on its own
      case c if c.getName.equals("org.hl7.fhir.r4.model.Type") => {

        property match {
          case None                                          => throw new Exception("unexpectedly missing property information!")
          case Some(prop) if prop.getTypeCode.equals("*")    => Seq() // ignore this case for now FIXME this results in being unable to register extensions
          case Some(prop) => {

            val types = getCleanedTypeCodes(prop).map(typeCode => (typeCode, typeCodeToClassMapping.get(typeCode).get))
            types.map{case (typeCode, clazz) => handleSingleClass(clazz, path.dropRight(1) :+ s"${path.last}[${typeCode}]")}.flatten
          }
        }
      }
      case c if Modifier.isAbstract(c.getModifiers) => Seq()
      case c => handleSingleClass(c, path)

    }
  }

  def handleSingleClass[T <: Base](clazz: Class[T], path: Seq[String]) = {
    val r = clazz.getDeclaredConstructor().newInstance()
    val cc = Hapi.getChildren(r)
    val collected = cc
      .filterNot(child => Seq("extension").contains(child.field.getName))
      .filterNot(child => Seq("id").contains(child.field.getName) && path.size!=1) // include top level IDs like Patient.id in the profile
      .filterNot(child => fhirCircuitBreak(child.field.getName, path))
      .collect {
        case child if classOf[java.util.List[_]].isAssignableFrom(child.field.getType) =>
          Try{
            val targs = child.field.getGenericType.asInstanceOf[ParameterizedType].getActualTypeArguments
            (targs.head.asInstanceOf[Class[_]], child.field.getName, child.property)
          }
        case child => child.field.getType
          Try((child.field.getType, child.field.getName, child.property))
      }

    val ccc: Seq[String] = collected
      .collect {
        case Success((c, n, p)) if classOf[Base].isAssignableFrom(c) =>
          children(c.asInstanceOf[Class[Base]], path :+ n, Some(p))
      }
      .flatten
    path.mkString(".") +: ccc
  }

}
