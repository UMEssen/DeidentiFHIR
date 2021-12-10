package de.ume.deidentifhir.util

import org.hl7.fhir.r4.model.Narrative.NarrativeStatus
import org.hl7.fhir.r4.model.{Annotation, HumanName, ImagingStudy, Narrative, Observation, Period, StringType}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.util.Date

//  extends AnyWordSpec with Matchers

class HapiTests extends AnyFunSuite {

  test("getCleanedTypeCodes") {
    val ann = new Annotation().setAuthor(new StringType("author"))
    val prop = ann.getNamedProperty("author")

    val types = Hapi.getCleanedTypeCodes(prop)
    types === Seq("Reference", "string")
  }

  test("getChildrenWithValue") {
    val hn = new HumanName()
    hn.addPrefix("Dr.")
    hn.addGiven("John")
    hn.addGiven("Alexander")
    hn.setFamily("Doe")
    hn.addSuffix("Jr.")
    hn.setUse(HumanName.NameUse.OFFICIAL)
    hn.setText("Lorem ipsum")
    hn.setPeriod(new Period().setStart(new Date(new Date().getTime - 123456)).setEnd(new Date()))

    Hapi.getChildrenWithValue(hn).map { case (fp, value) =>
      println(s"${fp.property.getName} [${fp.field.getType}]: $value")
    }
  }

//  test("getChildrenWithValue: narrative") {
//    val narrative = new Narrative().setStatus(NarrativeStatus.GENERATED)
//    narrative.setDivAsString("test")
//    assert(Hapi.getChildrenWithValue(narrative).size == 2)
//
//    val xhtml = narrative.getDiv
//  }
}
