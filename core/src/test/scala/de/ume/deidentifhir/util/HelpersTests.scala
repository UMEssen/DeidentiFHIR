package de.ume.deidentifhir.util

import ca.uhn.fhir.model.api.TemporalPrecisionEnum
import org.hl7.fhir.r4.model.{DateTimeType, DateType}
import org.scalatest.funsuite.AnyFunSuite

class HelpersTests extends AnyFunSuite {

  // The bug used to be: Helpers.constantDateShift(d, DAY, n) actually shifted
  // by n MONTHS because TemporalPrecisionEnum.DAY.ordinal() == 2 and
  // Calendar.MONTH == 2. These tests pin the corrected unit mapping.

  test("DAY shift moves the calendar by days, not months") {
    val d = new DateType("2024-01-15")
    val out = Helpers.constantDateShift(d, TemporalPrecisionEnum.DAY, -7)
    assert(out.getValueAsString == "2024-01-08")
  }

  test("DAY shift crosses month boundaries") {
    val d = new DateType("2024-01-15")
    val out = Helpers.constantDateShift(d, TemporalPrecisionEnum.DAY, 30)
    assert(out.getValueAsString == "2024-02-14")
  }

  test("MONTH shift") {
    val d = new DateType("2024-01-15")
    val out = Helpers.constantDateShift(d, TemporalPrecisionEnum.MONTH, -1)
    assert(out.getValueAsString == "2023-12-15")
  }

  test("YEAR shift") {
    val d = new DateType("2024-01-15")
    val out = Helpers.constantDateShift(d, TemporalPrecisionEnum.YEAR, 5)
    assert(out.getValueAsString == "2029-01-15")
  }

  test("MINUTE shift") {
    val d = new DateTimeType("2024-01-15T12:30:00Z")
    val out = Helpers.constantDateShift(d, TemporalPrecisionEnum.MINUTE, 45).asInstanceOf[DateTimeType]
    val deltaMs = out.getValue.getTime - d.getValue.getTime
    assert(deltaMs == 45L * 60 * 1000)
  }

  test("SECOND shift") {
    val d = new DateTimeType("2024-01-15T12:30:00Z")
    val out = Helpers.constantDateShift(d, TemporalPrecisionEnum.SECOND, 90).asInstanceOf[DateTimeType]
    val deltaMs = out.getValue.getTime - d.getValue.getTime
    assert(deltaMs == 90L * 1000)
  }

  test("MILLI shift") {
    val d = new DateTimeType("2024-01-15T12:30:00.000Z")
    val out = Helpers.constantDateShift(d, TemporalPrecisionEnum.MILLI, 250).asInstanceOf[DateTimeType]
    val deltaMs = out.getValue.getTime - d.getValue.getTime
    assert(deltaMs == 250L)
  }

  test("does not mutate the input") {
    val d = new DateType("2024-01-15")
    val before = d.getValueAsString
    val _ = Helpers.constantDateShift(d, TemporalPrecisionEnum.DAY, -7)
    assert(d.getValueAsString == before, "input was mutated")
  }
}
