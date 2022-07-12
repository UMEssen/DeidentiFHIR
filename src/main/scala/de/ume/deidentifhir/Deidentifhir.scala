package de.ume.deidentifhir

import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging
import de.ume.deidentifhir.Deidentifhir.DeidentifhirHandler
import de.ume.deidentifhir.util.AlwaysReturnValueMap
import de.ume.deidentifhir.util.DeidentifhirUtils.{mergeHandlers, resourceMatchesFhirPath}
import de.ume.deidentifhir.util.Hapi._
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
  // 4. a map of strings that is static for each call of deidentify and can store a static context for the current de-identification run
  type DeidentifhirHandler[T <: Any] = (Seq[String], T, Seq[Base], Map[String, String]) => T

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

  /**
   * @param resource Either a resource or a whole bundle that should be de-identified.
   * @return The de-identified resource or bundle. If all elements in a resource were removed, this method will return
   *         <code>null</code>. If all resources in a bundle were removed, this method will return an empty bundle.
   */
  def deidentify(resource: Resource): Resource = {
    deidentify(resource, Map())
  }

  /**
   * @param resource Either a resource or a whole bundle that should be de-identified.
   * @param staticContext An immutable map that is passed to each subsequent de-identification call to pass some static context.
   * @return The de-identified resource or bundle. If all elements in a resource were removed, this method will return
   *         <code>null</code>. If all resources in a bundle were removed, this method will return an empty bundle.
   */
  def deidentify(resource: Resource, staticContext: Map[String, String]): Resource = {
    resource match {
      case b: Bundle =>
        val deidentifiedBundle = new Bundle()
        b.getEntry.asScala
          .map(_.getResource)
          .map(deidentify(_,staticContext))
          .filter(_!=null) // remove entries if the resource was removed altogether
          .map(deidentifiedResource => {
            new BundleEntryComponent().setResource(deidentifiedResource)
          })
          .foreach(deidentifiedBundle.addEntry)
        deidentifiedBundle
      case r =>
        deidentifyWrapper(Seq(r.fhirType), r, Seq(), staticContext).asInstanceOf[Resource]
    }
  }

  // TODO applyHandlers is only called on primitive types! -> change from Any to PrimitiveType[_]
  private def applyHandlers(path: Seq[String], value: Any, context: Seq[Base], staticContext: Map[String, String]): Any = {

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
      .map(_.map(_.foldLeft(value)((accumulator, handler) => handler(path, accumulator, context, staticContext))).getOrElse(value))
      .orElse(Option(value))
      // now apply path handlers
      .flatMap { newvalue =>
        mergedPathHandlers
          .map(_.map(_.foldLeft(newvalue)((accumulator, handler) => handler(path, accumulator, context, staticContext))).getOrElse(newvalue))
          .orElse(None)
      }
      // if not captured by any handler setting 'null' will remove that property from a HAPI model
      .orNull
  }

  private def deidentifyWrapper(path: Seq[String], b: Base, context: Seq[Base], staticContext: Map[String, String]): Base = {

    // create empty instance to fill with white-listed attributes
    val emptyBase = b.getClass.getConstructor().newInstance()

    var anyFieldSet = false;

    val childrenWithValue = getChildrenWithValue(b)
    val mappedChildrenWithValue = childrenWithValue.map { case (fp, value) => (path :+ toPathElement(fp.property, value), value, fp.field)}
      mappedChildrenWithValue.foreach {
        case (path, value, field) =>
          val innerRes = deidentifyInner(path, value, context :+ b, staticContext)
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

  private def deidentifyInner(path: Seq[String], value: Any, context: Seq[Base], staticContext: Map[String, String]): Any = {
    value match {
      case v: PrimitiveType[_] =>
        val deidentifiedValue = applyHandlers(path, v.copy(), context, staticContext).asInstanceOf[PrimitiveType[_]]
        // the extensions that are associated with a primitive type need to be handled separately
        val deidentifiedExtensions = v.getExtension.asScala
          .map(deidentifyWrapper(path :+ "extension", _, context, staticContext).asInstanceOf[Extension])
          .filterNot(_ == null)
          .asJava

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
        deidentifyWrapper(path, v, context, staticContext)
      case v: java.util.List[_] =>
          val list = v.asScala
            .map(deidentifyInner(path, _, context, staticContext))
            .filterNot(_ == null)
            .toList.asJava
          if(list.isEmpty) {
            // This can either be the case if
            // 1. the list was already empty, which can happen if the resource was parsed with HAPI FHIR,
            // 2. or if all elements were removed by deidentifyInner
            null
          } else {
            list
          }
      case _ => throw new Exception("Unexpected input!")
    }
  }

}
