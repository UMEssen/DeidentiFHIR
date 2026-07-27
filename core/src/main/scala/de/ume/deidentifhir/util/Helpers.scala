package de.ume.deidentifhir.util

import ca.uhn.fhir.model.api.TemporalPrecisionEnum
import org.hl7.fhir.r4.model.BaseDateTimeType

import java.time.ZonedDateTime
import java.util.Calendar
import scala.concurrent.duration.FiniteDuration
import scala.jdk.DurationConverters._
import org.dcm4che3.util.UIDUtils

object Helpers {

  def constantDateShift(date: BaseDateTimeType, unit: TemporalPrecisionEnum, value: Int): BaseDateTimeType = {
    val shiftedDate = date.copy().asInstanceOf[BaseDateTimeType]
    shiftedDate.add(calendarFieldFor(unit), value)
    shiftedDate
  }

  /** Map HAPI's [[TemporalPrecisionEnum]] to the matching `java.util.Calendar`
   * field constant, which is what [[BaseDateTimeType.add(int, int)]] expects.
   *
   * Calling `BaseDateTimeType.add(unit.ordinal(), value)` was wrong: the
   * enum's ordinals (YEAR=0, MONTH=1, DAY=2, …) collide with Calendar's field
   * constants (ERA=0, YEAR=1, MONTH=2, DATE=5, …) only at YEAR/MONTH; DAY in
   * particular shifted by months instead of days.
   */
  private def calendarFieldFor(unit: TemporalPrecisionEnum): Int = unit match {
    case TemporalPrecisionEnum.YEAR   => Calendar.YEAR
    case TemporalPrecisionEnum.MONTH  => Calendar.MONTH
    case TemporalPrecisionEnum.DAY    => Calendar.DAY_OF_MONTH
    case TemporalPrecisionEnum.MINUTE => Calendar.MINUTE
    case TemporalPrecisionEnum.SECOND => Calendar.SECOND
    case TemporalPrecisionEnum.MILLI  => Calendar.MILLISECOND
    case other                        =>
      throw new IllegalArgumentException(s"unsupported precision: $other")
  }

  def constantDateShift(date: ZonedDateTime, duration: FiniteDuration): ZonedDateTime = {
    date.minus(duration.toJava)
  }

  def jitter(date: BaseDateTimeType, max: Int = 10, unit: TemporalPrecisionEnum = TemporalPrecisionEnum.MILLI): BaseDateTimeType = {
    constantDateShift(date, unit, ((Math.random() - 0.5) * 2.0 * max).toInt)
  }

  def sha256(s: String): String = {
    import java.nio.charset.StandardCharsets
    import java.security.MessageDigest
    MessageDigest.getInstance("SHA-256")
      .digest(s.getBytes(StandardCharsets.UTF_8))
      .map("%02X" format _).mkString
  }

  /**
   * Performs UID de-identification using Dcm4Che.
   * Transparently handles 'urn:oid:' prefixes.
   * @param uid
   * @return
   */
  def dicomUID(uid: String): String = {
    uid match {
      case s"urn:oid:${realUid}" => s"urn:oid:${UIDUtils.createNameBasedUID(realUid.getBytes)}"
      case _ => UIDUtils.createNameBasedUID(uid.getBytes)
    }
  }
}
