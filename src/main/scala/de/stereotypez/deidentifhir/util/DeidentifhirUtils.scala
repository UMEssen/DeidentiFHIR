package de.stereotypez.deidentifhir.util

import de.stereotypez.deidentifhir.Deidentifhir.DeidentifhirHandler
import de.stereotypez.deidentifhir.{ApplyAlways, FhirPath, IdentifierSystemFhirPath, ProfileFhirPath, ResourceExistsPath}
import org.hl7.fhir.instance.model.api.{IBaseHasExtensions, IBaseHasModifierExtensions, IBaseResource}
import org.hl7.fhir.r4.model.{BaseResource, Identifier, Resource}

import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.reflect.ClassTag
import scala.reflect.internal.ClassfileConstants.instanceof

object DeidentifhirUtils {

  //e.g. "Observation.meta.profile contains 'https://www.medizininformatik-initiative.de/fhir/core/modul-labor/StructureDefinition/ObservationLab'"
  def determinePattern(fhirPathString: String) = {
    val profileRegex = "(.*).meta.profile contains '(.*)'".r
    val resourceExists = "(.*).exists\\(\\)".r
    //e.g. Patient.identifier.system contains 'urn:oid:1.2.36.146.595.217.0.1'
    val identifierSystemRegex = "(.*).identifier.system contains '(.*)'".r

    fhirPathString match {
      case profileRegex(resourceType, canonicalProfile)           => ProfileFhirPath(resourceType, canonicalProfile)
      case resourceExists(resourceType)                           => ResourceExistsPath(resourceType)
      case identifierSystemRegex(resourceType, canonicalProfile)  => IdentifierSystemFhirPath(resourceType, canonicalProfile)
      case _                                                      => throw new Exception(s"Pattern '$fhirPathString' cannot be parsed.")
    }
  }

  def resourceMatchesFhirPath(resource: Resource, fhirPath: FhirPath): Boolean = fhirPath match {
    case ProfileFhirPath(resourceType, canonicalProfile) => {
      resource.getResourceType.toString.equals(resourceType) && resource.getMeta.hasProfile(canonicalProfile)
    }
    case IdentifierSystemFhirPath(resourceType, identifierSystem) => {
      // since there is no common base class that defines functions like hasIdentifier and getIdentifier, we need to use
      // access via reflections here
      if(resource.getResourceType.toString.equals(resourceType)) {
        val field = Reflection.getAccessibleField(resource.getClass, "identifier")
        val identifiers = field.get(resource).asInstanceOf[java.util.List[Identifier]]
        if(identifiers==null) {
          false
        } else {
          val matchedIdentifier = identifiers.asScala.find(identifier => {identifier.hasSystem && identifier.getSystem.equals(identifierSystem)})
          matchedIdentifier.isDefined
        }
      } else {
        false
      }
    }
    case ResourceExistsPath(resourceType) => {
      resource.getResourceType.toString.equals(resourceType)
    }
    case ApplyAlways() => true
    case _ => throw new Exception(s"Received unexpected pattern: $fhirPath")
  }

  // TODO this code can probably be improved and made more idiomatic
  def mergeHandlers(list: Seq[Option[Option[Seq[DeidentifhirHandler[Any]]]]]): Option[Option[Seq[DeidentifhirHandler[Any]]]] = {
    list.filter(_.isDefined) match {
      case Nil          => None
      case filteredList => Some({
        val inner = filteredList.map(_.get)
        inner.filter(_.isDefined) match {
          case Nil                => None
          case innerFilteredList  => Some(innerFilteredList.map(_.get).flatten)
        }
      })
    }
  }
}
