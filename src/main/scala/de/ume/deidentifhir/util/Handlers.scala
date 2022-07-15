package de.ume.deidentifhir.util

import ca.uhn.fhir.model.api.TemporalPrecisionEnum
import de.ume.deidentifhir.Deidentifhir.DeidentifhirHandler
import org.hl7.fhir.r4.model._

object Handlers {

  /**
   * This handler returns the unchanged input value.
   */
  val keepHandler: Option[DeidentifhirHandler[Base]] = None

  /**
   * Truncates the postal code to the first three digits, if the given postal code is five digits long. Otherwise, the postal code is removed altogether.
   */
  def generalizePostalCode(path: Seq[String], postalCode: StringType, context: Seq[Base], staticContext: Map[String, String]): StringType = {
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
   * Set the day of the month to the 15th according to the MII/SMITH pseudonymization concept.
   */
  def generalizeDateHandler(path: Seq[String], date: DateType, context: Seq[Base], staticContext: Map[String, String]): DateType = {

    val precision = date.getPrecision
    precision match {
      case TemporalPrecisionEnum.YEAR   => // do nothing if the precision is lower than DAY
      case TemporalPrecisionEnum.MONTH  => // do nothing if the precision is lower than DAY
      case TemporalPrecisionEnum.DAY    => date.setDay(15) // the day field is 1-indexed!
      case TemporalPrecisionEnum.MINUTE => throw new Exception("Unexpected precision for object of type DateType!")
      case TemporalPrecisionEnum.SECOND => throw new Exception("Unexpected precision for object of type DateType!")
      case TemporalPrecisionEnum.MILLI  => throw new Exception("Unexpected precision for object of type DateType!")
      case _                            => throw new Exception("Encountered an unknown precision!")
    }

    // even though the precision might only be DAY or lower, the object can hold a more specific time stamp:
    date.setHour(0)
    date.setMinute(0)
    date.setSecond(0)
    date.setMillis(0)

    date
  }

  val patientIdentifierKey = "patientIdentifier"

  /**
   * Shift the date by given milliseconds (positive or negative).
   */
  def shiftDateHandler(shiftDateProvider: ShiftDateProvider)(path: Seq[String], date: BaseDateTimeType, context: Seq[Base], staticContext: Map[String, String]): BaseDateTimeType = {
    staticContext.contains(patientIdentifierKey) match {
      case true => {
        val dateValue = date.getValue
        dateValue.setTime(dateValue.getTime + shiftDateProvider.getDateShiftingValueInMillis(staticContext.get(patientIdentifierKey).get))
        date match {
          case _: DateType      => new DateType(dateValue, date.getPrecision)
          case _: DateTimeType  => new DateTimeType(dateValue, date.getPrecision, date.getTimeZone)
          case _: InstantType   => new InstantType(dateValue, date.getPrecision, date.getTimeZone)
        }
      }
      case false => throw new Exception("The shiftDateHandler requires a patientIdentifier in the staticContext!")
    }
  }

  /**
   * Replaces the given string with a predefined static string.
   */
  def stringReplacementHandler(staticString: String)(path: Seq[String], string: StringType, context: Seq[Base], staticContext: Map[String, String]) = {
    new StringType(staticString)
  }

  def referenceReplacementHandler(idReplacementProvider: IDReplacementProvider)(path: Seq[String], reference: StringType, context: Seq[Base], staticContext: Map[String, String]) = {
    reference.getValue match {
      case s"$resourceType/$idPart" => new StringType(s"$resourceType/${idReplacementProvider.getIDReplacement(resourceType, idPart)}")
      case _  => throw new Exception("unexpected reference format. only relative references are supported right now!")
    }
  }

  def idReplacementHandler(idReplacementProvider: IDReplacementProvider)(path: Seq[String], id: IdType, context: Seq[Base], staticContext: Map[String, String]) = {
    val resourceType = context.head.asInstanceOf[Resource].getResourceType.toString
    new IdType(id.getResourceType, idReplacementProvider.getIDReplacement(resourceType, id.getIdPart))
  }

  def identifierValueReplacementHandler(identifierValueReplacementProvider: IdentifierValueReplacementProvider, acceptNoSystem: Boolean)(path: Seq[String], value: StringType, context: Seq[Base], staticContext: Map[String, String]) = {

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

  /**
   * In addition to the functionality of the referenceReplacementHandler this handler also modifies conditional references
   * which are only allowed in transaction bundles (https://www.hl7.org/fhir/http.html#trules).
   *
   * Right now, only search URIs that specify a system and an identifier are allowed: Patient?identifier=mySystem|12345
   */
  def conditionalReferencesReplacementHandler(idReplacementProvider: IDReplacementProvider, identifierValueReplacementProvider: IdentifierValueReplacementProvider)(path: Seq[String], reference: StringType, context: Seq[Base]): StringType = {
    reference.getValue match {
      case s"$resourceType?identifier=$identifierSystem|$identifierValue" => new StringType(s"$resourceType?identifier=$identifierSystem|${identifierValueReplacementProvider.getValueReplacement(identifierSystem, identifierValue)}")
      case _                                                              => referenceReplacementHandler(idReplacementProvider)(path, reference, context)
    }
  }
}
