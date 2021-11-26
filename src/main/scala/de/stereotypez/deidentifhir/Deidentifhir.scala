package de.stereotypez.deidentifhir

import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging
import de.stereotypez.deidentifhir.Deidentifhir.DeidentifhirHandler
import de.stereotypez.deidentifhir.util.AlwaysReturnValueMap
import de.stereotypez.deidentifhir.util.DeidentifhirUtils.{mergeHandlers, resourceMatchesFhirPath}
import de.stereotypez.deidentifhir.util.Hapi._
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent
import org.hl7.fhir.r4.model._

import scala.jdk.CollectionConverters._

sealed abstract class FhirPath
case class ProfileFhirPath(resourceType: String, canonicalProfile: String) extends FhirPath
case class IdentifierSystemFhirPath(resourceType: String, identifierSystem: String) extends FhirPath
case class ResourceExistsPath(resourceType: String) extends FhirPath
case class ApplyAlways() extends FhirPath

case class Module(pattern: FhirPath,
                  pathHandlers: Map[String, Option[Seq[DeidentifhirHandler[Any]]]],
                  typeHandlers: Map[Class[_], Option[Seq[DeidentifhirHandler[Any]]]])

object Deidentifhir {
  // each handler gets passed:
  // 1. the path to the current element
  // 2. the current element
  // 3. a sequence of all elements from the root to the current element
  type DeidentifhirHandler[T <: Any] = (Seq[String], T, Seq[Base]) => T

  // TODO deduplicate code with the other apply method
  def apply(config: Config): Deidentifhir = {
    // TODO check config version
    val moduleConfigs = config.getObject("modules")
    val moduleKeys = moduleConfigs.keySet()
    val modules = moduleKeys.asScala.toSeq.map(key => { ModuleBuilder(moduleConfigs.toConfig.getConfig(key)).build()})

    new Deidentifhir(modules)
  }

  def apply(config: Config, registry: Registry): Deidentifhir = {
    // TODO check config version
    val moduleConfigs = config.getObject("modules")
    val moduleKeys = moduleConfigs.keySet()
    val modules = moduleKeys.asScala.toSeq.map(key => { ModuleBuilder(moduleConfigs.toConfig.getConfig(key), registry).build()})

    new Deidentifhir(modules)
  }

  def buildKeepAll() = {
    val keepMap = new AlwaysReturnValueMap()
    val keepModule = Module(pattern = ApplyAlways(), keepMap, Map())
    (new Deidentifhir(Seq(keepModule)), keepMap)
  }
}

class Deidentifhir(modules: Seq[Module]) extends LazyLogging {

  def deidentify(resource: Resource): Resource = {
    resource match {
      case b: Bundle =>
        val deidentifiedBundle = new Bundle()
        b.getEntry.asScala
          .map(_.getResource)
          .map(deidentify) // TODO filter for null?
          .map(deidentifiedResource => {
            new BundleEntryComponent().setResource(deidentifiedResource)
          })
          .foreach(deidentifiedBundle.addEntry)
        deidentifiedBundle
      case r =>
        deidentifyWrapper(Seq(r.fhirType), r, Seq()).asInstanceOf[Resource]
    }
  }

  // TODO applyHandlers is only called on primitive types! -> change from Any to PrimitiveType[_]
  private def applyHandlers(path: Seq[String], value: Any, context: Seq[Base]): Any = {

    assert(value.isInstanceOf[PrimitiveType[_]])

    // only use modules whose pattern matches the current resource
    val resource = context.head.asInstanceOf[Resource]
    val applicableModules = modules.filter(module => {resourceMatchesFhirPath(resource, module.pattern)})
    // get all handlers for the current path from all applicable modules
    val mergedPathHandlers = mergeHandlers(applicableModules.map(_.pathHandlers).map(_.get(path.mkString("."))))
    // get all handlers for the current type from all applicable modules
    val mergedTypeHandlers= mergeHandlers(applicableModules.map(_.typeHandlers).map(_.get(value.getClass)))

    // type handlers have precedence
    mergedTypeHandlers
      .map(_.map(_.foldLeft(value)((accumulator, handler) => handler(path, accumulator, context))).getOrElse(value))
      .orElse(Option(value))
      // now apply path handlers
      .flatMap { newvalue =>
        mergedPathHandlers
          .map(_.map(_.foldLeft(newvalue)((accumulator, handler) => handler(path, accumulator, context))).getOrElse(newvalue))
          .orElse(None)
      }
      // if not captured by any handler setting 'null' will remove that property from a HAPI model
      .orNull
  }

  private def deidentifyWrapper(path: Seq[String], b: Base, context: Seq[Base]): Base = {

    // create empty instance to fill with white-listed attributes
    val emptyBase = b.getClass.getConstructor().newInstance()

    var anyFieldSet = false;

    val childrenWithValue = getChildrenWithValue(b)
    val mappedChildrenWithValue = childrenWithValue.map { case (fp, value) => (path :+ toPathElement(fp.property, value), value, fp.field)}
      mappedChildrenWithValue.foreach {
        case (path, value, field) =>
          val innerRes = deidentifyInner(path, value, context :+ b)
          if(innerRes != null) {
            anyFieldSet = true
            field.set(emptyBase, innerRes)
          }
      }

    if(anyFieldSet)
      emptyBase
    else
      null
  }

  private def deidentifyInner(path: Seq[String], value: Any, context: Seq[Base]): Any = {
    value match {
      case v: PrimitiveType[_] =>
        val deidentifiedValue = applyHandlers(path, v.copy(), context).asInstanceOf[PrimitiveType[_]]
        // the extensions that are associated with a primitive type need to be handled separately
        val deidentifiedExtensions = v.getExtension.asScala.map(deidentifyWrapper(path :+ "extension", _, context).asInstanceOf[Extension]).asJava

        if(deidentifiedValue==null) {
          if(deidentifiedExtensions.isEmpty) {
            // the primitive type was removed and the extensions as well
            null
          } else {
            // the primitive type was removed but the extensions should be kept
            val emptyValue = v.getClass.getConstructor().newInstance()
            emptyValue.setExtension(deidentifiedExtensions)
            emptyValue
          }
        } else {
          deidentifiedValue.setExtension(deidentifiedExtensions)
          deidentifiedValue
        }

      case v: Base =>
        // recurse
        deidentifyWrapper(path, v, context)
      case v: java.util.List[_] =>
        if(v.isEmpty) {
          // remove empty lists altogether. which might be the case if the resource was parsed with HAPI FHIR
          null
        } else {
          v.asScala
            .map(deidentifyInner(path, _, context))
            .filterNot(_ == null)
            .toList.asJava
        }
      case _ => throw new Exception("Unexpected input!")
    }
  }

}