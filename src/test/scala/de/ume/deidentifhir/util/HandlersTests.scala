package de.ume.deidentifhir.util

import org.hl7.fhir.r4.model.{Base, IdType, Patient}
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
