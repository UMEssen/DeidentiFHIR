package de.stereotypez.deidentifhir.util

import de.stereotypez.deidentifhir.Deidentifhir.DeidentifhirHandler
import org.hl7.fhir.instance.model.api.IPrimitiveType
import org.hl7.fhir.r4.model.{Base, DateType, IdType, Identifier, Resource, StringType}
object Handlers {

  /**
   * This handler returns the unchanged input value.
   */
  val keepHandler: Option[DeidentifhirHandler[Base]] = None

  /**
   * Truncates the postal code to the first three digits, if the given postal code is five digits long. Otherwise, the postal code is removed altogether.
   */
  def generalizePostalCode(path: Seq[String], postalCode: StringType, context: Seq[Base]): StringType = {
    postalCode.getValue match {
      case value if value.length == 5 => {
        postalCode.setValue(value.substring(0, 3))
        postalCode
      }
      case _ => null
    }
  }
  val generalizePostalCodeHandler: Option[DeidentifhirHandler[StringType]] = Some(generalizePostalCode)

  /**
   * Datum auf 15. des Monats setzen
   */
  def generalizeDate(path: Seq[String], date: DateType, context: Seq[Base]): DateType = {
    date.setDay(15) // TODO check if this is 0 or 1 based
    // TODO DateType can contain a time as well! delete this as well!
    // println(date.getHour())
    date
  }
  val generalizeDateHandler: Option[DeidentifhirHandler[DateType]] = Some(generalizeDate)

//  /**
//   * TODO
//   */
//  def generalizeDeceased(path: Seq[String], deceased: Type): Type = {
//    deceased match {
//      case value: BooleanType => value
//      case value: DateType => value
//      case _  => throw new Exception("received unexpected type!")
//    }
//  }
//  val generalizeDeceasedHandler: Option[DeidentifhirHandler[Type]] = Some(generalizeDeceased)

  def referenceReplacementHandler(idReplacementProvider: IDReplacementProvider)(path: Seq[String], reference: StringType, context: Seq[Base]) = {
    reference.getValue match {
      case s"$resourceType/$idPart" => new StringType(s"$resourceType/${idReplacementProvider.getIDReplacement(resourceType, idPart)}")
      case _  => throw new Exception("unexpected reference format. only relative references are supported right now!")
    }
  }

  def idReplacementHandler(idReplacementProvider: IDReplacementProvider)(path: Seq[String], id: IdType, context: Seq[Base]) = {
    val resourceType = context.head.asInstanceOf[Resource].getResourceType.toString
    new IdType(id.getResourceType, idReplacementProvider.getIDReplacement(resourceType, id.getIdPart))
  }

  def identifierValueReplacementHandler(identifierValueReplacementProvider: IdentifierValueReplacementProvider, acceptNoSystem: Boolean)(path: Seq[String], value: StringType, context: Seq[Base]) = {

    // the value is not unique on its own. therefore, we need to determine the identifiers system and use this as a prefix.
    val identifier = context.last.asInstanceOf[Identifier]

    val system =
    if(identifier.hasSystem && identifier.getSystem!=null) {
      identifier.getSystem
    } else {
      if(acceptNoSystem) {
        "<no_system>"
      } else {
        throw new RuntimeException(s"Identifier $identifier is missing a system, which is required for performing this replacement.")
      }
    }

    new StringType(identifierValueReplacementProvider.getValueReplacement(system, value.getValue()))
  }
}
