package de.ume.deidentifhir.util

import ca.uhn.fhir.model.api.TemporalPrecisionEnum
import de.ume.deidentifhir.Deidentifhir.DeidentifhirHandler
import org.hl7.fhir.r4.model._

import java.util.TimeZone
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
   * Set the day of the month to the 15th according to the MII/SMITH pseudonymization concept.
   */
  def generalizeDateHandler(path: Seq[String], date: DateType, context: Seq[Base]): DateType = {

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


  /**
   * Shift the date by given milliseconds (positive or negative).
   */
  def shiftDateHandler(dateShiftValueProvider: ShiftDateProvider)(path: Seq[String], date: BaseDateTimeType, context: Seq[Base]): BaseDateTimeType = {
    val dateValue = date.getValue
    dateValue.setTime(dateValue.getTime + dateShiftValueProvider.getDateShiftingValueInMillis())
    date match {
      case _: DateType      => new DateType(dateValue, date.getPrecision)
      case _: DateTimeType  => new DateTimeType(dateValue, date.getPrecision, date.getTimeZone)
      case _: InstantType   => new InstantType(dateValue, date.getPrecision, date.getTimeZone)
    }
  }

  /**
   * Replaces the given string with a predefined static string.
   */
  def stringReplacementHandler(staticString: String)(path: Seq[String], string: StringType, context: Seq[Base]) = {
    new StringType(staticString)
  }

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
