package de.stereotypez.deidentifhir.util

import ca.uhn.fhir.model.api.TemporalPrecisionEnum
import org.hl7.fhir.r4.model.BaseDateTimeType

import scala.jdk.DurationConverters._
import java.time.ZonedDateTime
import scala.concurrent.duration.FiniteDuration
import org.dcm4che3.util.{TagUtils, UIDUtils}

object Helpers {

  def constantDateShift(date: BaseDateTimeType, unit: TemporalPrecisionEnum, value: Int): BaseDateTimeType = {
    val shiftedDate = date.copy().asInstanceOf[BaseDateTimeType]
    shiftedDate.add(unit.ordinal(), value)
    shiftedDate
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
