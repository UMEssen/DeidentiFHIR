package de.ume.deidentifhir.util

import org.hl7.fhir.r4.model.{Base, Property, Type}

import java.lang.reflect.Field
import javax.lang.model.SourceVersion
import scala.jdk.CollectionConverters.CollectionHasAsScala

case class FhirProperty(property: Property, field: Field)

object Hapi {

  // Reference(Practitioner|Patient|RelatedPerson|Organization)|string
  // ->
  // [Reference, string]
  def getCleanedTypeCodes(property: Property): Seq[String] = {
    // remove all class information between parenthesis using a non-greedy matcher, before splitting
    property.getTypeCode.replaceAll("\\(.*?\\)","").split('|')
  }

  def toPathElement(property: Property, value: Any): String = {
    property.getName match {
      case s"$n[x]" => s"$n[${value.asInstanceOf[Type].fhirType()}]"
      case n        => n
    }
  }

  def nameToField(name: String): String = {
    name match {
      case s"$n[x]" => n
      case n if SourceVersion.isKeyword(n) => n + "_"
      case n => n
    }
  }

  def getChildren(r: Base): Seq[FhirProperty] = {
    r.children().asScala.toSeq
      .map { p => FhirProperty(p, Reflection.getAccessibleField(r.getClass, nameToField(p.getName)))}
  }

  def getChildrenWithValue(r: Base): Map[FhirProperty, Any] = {
    getChildren(r)
      .filter(_.field.get(r) != null)
      .map(fp => fp -> fp.field.get(r))
      .toMap
  }

  def fhirCircuitBreak(element: String, path: Seq[String]): Boolean = {
    element match {
      case "identifier" if path.endsWith(Seq("identifier", "assigner")) => true
      case "extension" if path.endsWith(Seq("extension")) => true
      case "provision" if path.endsWith(Seq("provision")) => true
      case "rule" if path.endsWith(Seq("rule")) => true
      case "contains" if path.endsWith(Seq("contains")) => true
      case "page" if path.endsWith(Seq("page")) => true
      case "instantiates" if path.endsWith(Seq("instantiates")) => true
      case "imports" if path.endsWith(Seq("imports")) => true
      case "import" if path.endsWith(Seq("import")) => true
      case "partOf" if path.endsWith(Seq("partOf")) => true
      case "partOf" if path.endsWith(Seq("partOf", "replaces")) => true
      case "derivedFrom" if path.endsWith(Seq("derivedFrom")) => true
      case "replaces" if path.endsWith(Seq("replaces")) => true
      case "instantiates" if path.endsWith(Seq("instantiates", "imports")) => true
      case "workflow" if path.endsWith(Seq("workflow")) => true
      case "id" if path.endsWith(Seq("id")) => true
      case "id" if path.endsWith(Seq("id", "extension")) => true
      case "url" if path.endsWith(Seq("id", "extension")) => true
      case "url" if path.endsWith(Seq("meta", "extension")) => true
      case "extension" if path.endsWith(Seq("extension", "url")) => true
      case "concept" if path.endsWith(Seq("concept")) => true
      case "section" if path.endsWith(Seq("section")) => true
      case "process" if path.endsWith(Seq("process", "step")) => true
      case "step" if path.endsWith(Seq("process","step","alternative")) => true
      case "packageItem" if path.endsWith(Seq("packageItem")) => true
      case "link" if path.endsWith(Seq("link", "target")) => true
      case "part" if path.endsWith(Seq("parameter", "part")) => true
      case "item" if path.endsWith(Seq("item")) => true
      case "synonym" if path.endsWith(Seq("synonym")) => true
      case "synonym" if path.endsWith(Seq("synonym", "translation")) => true
      case "translation" if path.endsWith(Seq("translation")) => true
      case "group" if path.endsWith(Seq("group")) => true
      case "application" if path.endsWith(Seq("application")) => true
      case "action" if path.endsWith(Seq("action")) => true
      case "item" if path.endsWith(Seq("item", "answer")) => true
      case _ => false
    }
  }
}
