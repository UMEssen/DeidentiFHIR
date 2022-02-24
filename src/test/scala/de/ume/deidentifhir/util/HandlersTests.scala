package de.ume.deidentifhir.util

import org.hl7.fhir.r4.model.{Base, DateType, Patient, StringType}
import org.scalatest.funsuite.AnyFunSuite

class HandlersTests extends AnyFunSuite {

//  // TODO move to HandlersTests
//  test("generalizeDate") {
//    val patient = new Patient()
//    patient.setBirthDate(new Date())
//    val pPatient : Patient = deidentifhir.deidentify(patient).asInstanceOf[Patient]
//    val cal = Calendar.getInstance()
//    cal.setTime(pPatient.getBirthDate)
//    assert(cal.get(Calendar.DAY_OF_MONTH) == 15)
//  }

//  test("generalizeDeceased") {
//
//    val pDeceasedBoolean = generalizeDeceased(Seq(), new BooleanType(true))
//    assert(pDeceasedBoolean.hasType("boolean"))
//
//    val pDeceasedDateType = generalizeDeceased(Seq(), new DateType())
//    // TODO assert(pDeceasedDateType.hasType("DateType"))
//
//    val thrown = intercept[Exception] {
//      generalizeDeceased(Seq(), new DateTimeType())
//    }
//    assert(thrown.getMessage === "received unexpected type!")
//  }

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
}
