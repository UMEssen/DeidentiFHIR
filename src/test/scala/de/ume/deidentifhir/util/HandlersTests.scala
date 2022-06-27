package de.ume.deidentifhir.util

import ca.uhn.fhir.context.FhirContext
import org.hl7.fhir.r4.model.{Base, DateTimeType, DateType, Identifier, InstantType, Observation, Patient, Reference, StringType}
import org.scalatest.funsuite.AnyFunSuite

class HandlersTests extends AnyFunSuite {

  test("testStringReplacementHandler") {
    val pString = Handlers.stringReplacementHandler("test")(Seq(), new StringType("toBeReplaced"), Seq())
    assert(pString.getValue=="test")
  }

  test("generalizeDateHandler") {
    assert(Handlers.generalizeDateHandler(Seq(), new DateType("1905-08-23"), Seq()).getDay==15)

    // check that the day is not set if the DateType has a lower precision
    assert(Handlers.generalizeDateHandler(Seq(), new DateType("1905-08"), Seq()).getDay==1)

    // remove high precision data
    val date = new DateType("1905-08-23")
    date.setHour(3)
    date.setMinute(5)
    date.setSecond(5)
    date.setMillis(111)
    val pDate = Handlers.generalizeDateHandler(Seq(), date, Seq())
    assert(pDate.getHour==0)
    assert(pDate.getMinute==0)
    assert(pDate.getSecond==0)
    assert(pDate.getMillis==0)
  }

  test("dateShiftValueReplacementHandler") {

    assert(Handlers.shiftDateHandler(new ShiftDateProvider {
      override def getDateShiftingValueInMillis(): java.lang.Long = 86399999 // not quite one day
    })(Seq(),new DateType("1998-08-08"),Seq()).toString.equals("DateType[1998-08-08]"))

    assert(Handlers.shiftDateHandler(new ShiftDateProvider {
      override def getDateShiftingValueInMillis(): java.lang.Long = 86400000 // not quite one day
    })(Seq(),new DateType("1998-08-08"),Seq()).toString.equals("DateType[1998-08-09]"))

    assert(Handlers.shiftDateHandler(new ShiftDateProvider {
      override def getDateShiftingValueInMillis(): java.lang.Long = 864000000 // 10 days
    })(Seq(),new DateType("1998-08-08"),Seq()).toString.equals("DateType[1998-08-18]"))

    assert(Handlers.shiftDateHandler(new ShiftDateProvider {
      override def getDateShiftingValueInMillis(): java.lang.Long = 31535999999L // not quite one year
    })(Seq(),new DateType("1998-08-08"),Seq()).toString.equals("DateType[1999-08-07]"))

    assert(Handlers.shiftDateHandler(new ShiftDateProvider {
      override def getDateShiftingValueInMillis(): java.lang.Long = 31536000000L // one year
    })(Seq(),new DateType("1998-08-08"),Seq()).toString.equals("DateType[1999-08-08]"))

    assert(Handlers.shiftDateHandler(new ShiftDateProvider {
      override def getDateShiftingValueInMillis(): java.lang.Long = 864000000 // 10 days
    })(Seq(),new DateTimeType("2022-06-23T13:46:02.665+00:00"),Seq()).toString.equals("DateTimeType[2022-07-03T13:46:02.665+00:00]"))

    assert(Handlers.shiftDateHandler(new ShiftDateProvider {
      override def getDateShiftingValueInMillis(): java.lang.Long = -10800000 // -3 hours
    })(Seq(),new InstantType("2022-06-23T13:46:02.665+05:00"),Seq()).toString.equals("InstantType[2022-06-23T10:46:02.665+05:00]"))

  }

  test("idReplacementHandler") {

    val patient = new Patient()
    patient.setId("test")

    Handlers.idReplacementHandler(new IDReplacementProvider {
      override def getIDReplacement(resourceType: String, id: String): String = {
        assert(resourceType.equals("Patient"))
        assert(id.equals("test"))
        "test"
      }
    })(Seq("patient"), patient.getIdElement, Seq(patient))
  }

  test("referenceReplacementHandler") {

    val idReplacementProvider = new IDReplacementProvider {
      override def getIDReplacement(resourceType: String, id: String): String = {
        assert(resourceType.equals("Patient"))
        assert(id.equals("123"))
        "456"
      }
    }

    val obs = new Observation()
    obs.setSubject(new Reference("Patient/123"))
    val pRef = Handlers.referenceReplacementHandler(idReplacementProvider)(Seq("observation"), obs.getSubject.getReferenceElement_, Seq(obs))
    assert(pRef.getValue.equals("Patient/456"))
  }
}
