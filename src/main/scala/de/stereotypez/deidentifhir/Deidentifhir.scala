package de.stereotypez.deidentifhir

import com.typesafe.scalalogging.LazyLogging
import de.stereotypez.deidentifhir.Deidentifhir.DeidentifhirHandler
import de.stereotypez.deidentifhir.util.Hapi._
import de.stereotypez.deidentifhir.util.Reflection.getAccessibleField
import org.hl7.fhir.instance.model.api.{IBaseDatatype, IBaseExtension, IBaseHasExtensions}
import org.hl7.fhir.r4.model.{Base, Bundle, PrimitiveType, Resource, Type}

import scala.jdk.CollectionConverters._

object Deidentifhir {
  type DeidentifhirHandler[T <: Any] = (Seq[String], T) => T
}

class Deidentifhir(pathHandlers: Map[String, Option[Seq[DeidentifhirHandler[Any]]]], typeHandlers: Map[Class[_], Option[Seq[DeidentifhirHandler[Any]]]]) extends LazyLogging {

  def deidentify(resource: Resource): Resource = {
    resource match {
      case b: Bundle =>
        b.getEntry.asScala
          .foreach(entry => entry.setResource(deidentify(entry.getResource)))
        b
      case r =>
        deidentifyWrapper(Seq(r.fhirType), r).asInstanceOf[Resource]
    }
  }

  private def applyHandlers(path: Seq[String], value: Any): Any = {
    // type handlers have precedence
    typeHandlers.get(value.getClass)
      .map(_.map(_.foldLeft(value)((accumulator, handler) => handler(path,accumulator))).getOrElse(value))
      .orElse(Option(value))
      // now apply path handlers
      .flatMap { newvalue =>
        pathHandlers.get(path.mkString("."))
          .map(_.map(_.foldLeft(newvalue)((accumulator, handler) => handler(path,accumulator))).getOrElse(newvalue))
          .orElse(None)
      }
      // if not captured by any handler setting 'null' will remove that property from a HAPI model
      .orNull
  }

  def deidentifyWrapper(path: Seq[String], b: Base): Base = {

    // create empty instance to fill with white-listed attributes
    val emptyBase = b.getClass.getConstructor().newInstance()

    getChildrenWithValue(b)
      .map { case (fp, value) => (path :+ fp.property.getName, value)}
      .foreach {
        case (path, value) =>
          getAccessibleField(b.getClass, path.last)
            .set(emptyBase, deidentifyInner(emptyBase, path)(value))
      }
    emptyBase
  }

  def deidentifyExtension(ext: IBaseExtension[_,_], path: Seq[String]): IBaseExtension[_, _] = {
    ext.setUrl(applyHandlers(path :+ "url", ext.getUrl).asInstanceOf[String])
    ext.setValue(applyHandlers(path :+ "value", ext.getValue).asInstanceOf[IBaseDatatype])
    ext.getExtension.asScala.foreach {
      case e: IBaseExtension[_, _] => deidentifyExtension(e, path :+ "extension")
      case e => throw new RuntimeException(s"Unexpected extension type ${e}")
    }
    ext
  }

  def deidentifyInner(emptyBase: Base, path: Seq[String])(value: Any): Any = value match {
    case v: Base if v.isPrimitive =>

      if (v.isInstanceOf[IBaseHasExtensions]) {
        v.asInstanceOf[IBaseHasExtensions].getExtension.asScala.foreach(deidentifyExtension(_, path :+ "extension"))
      }

      applyHandlers(path, v)
    case v: Base =>

      if (v.isInstanceOf[IBaseHasExtensions]) {
        v.asInstanceOf[IBaseHasExtensions].getExtension.asScala.foreach(deidentifyExtension(_, path :+ "extension"))
      }

      // recurse
      deidentifyWrapper(path, v)
    case v: java.util.List[_] =>
      v.asScala
        .map(deidentifyInner(emptyBase, path)(_))
        .toList.asJava
  }

}