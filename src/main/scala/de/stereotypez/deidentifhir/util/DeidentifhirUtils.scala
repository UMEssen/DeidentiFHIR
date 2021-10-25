package de.stereotypez.deidentifhir.util

import de.stereotypez.deidentifhir.Deidentifhir.DeidentifhirHandler
import de.stereotypez.deidentifhir.{ApplyAlways, FhirPath, ProfileFhirPath, ResourceExistsPath}
import org.hl7.fhir.r4.model.Resource

object DeidentifhirUtils {

  //e.g. "Observation.meta.profile contains 'https://www.medizininformatik-initiative.de/fhir/core/modul-labor/StructureDefinition/ObservationLab'"
  def determinePattern(fhirPathString: String) = {
    val profileRegex = "(.*).meta.profile contains '(.*)'".r
    val resourceExists = "(.*).exists\\(\\)".r
    fhirPathString match {
      case profileRegex(resourceType, canonicalProfile) => ProfileFhirPath(resourceType, canonicalProfile)
      case resourceExists(resourceType)                 => ResourceExistsPath(resourceType)
      case _                                            => throw new Exception(s"Pattern '$fhirPathString' cannot be parsed.")
    }
  }

  def resourceMatchesFhirPath(resource: Resource, fhirPath: FhirPath): Boolean = fhirPath match {
    case ProfileFhirPath(resourceType, canonicalProfile) => {
      resource.getResourceType.toString.equals(resourceType) && resource.getMeta.hasProfile(canonicalProfile)
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
