package de.ume.deidentifhir
import com.typesafe.config.ConfigFactory
import de.ume.deidentifhir.util.{Handlers, IDReplacementProvider}
import org.hl7.fhir.r4.model.Patient
import org.scalatest.funsuite._

class BuilderTests extends AnyFunSuite {

  class THS extends IDReplacementProvider() {
    override def getIDReplacement(domain: String, id: String): String = "dummy"
  }

  test("config with handlers") {

    val config = ConfigFactory.parseString(
    """
    |deidentiFHIR.profile.version=0.2
    |modules = {
    |  module1: {
    |    pattern = "Patient.exists()"
    |    base = [
    |      "Patient.active",
    |      "Patient.address.postalCode",
    |      "Patient.id",
    |    ]
    |    paths = {
    |      # Register a handler without any parameters
    |      "Patient.address.postalCode" : { handler = generalizePostalCode }
    |      # Register a handler against the base
    |      "*.id" : { handler = replaceWithPseudonym }
    |    }
    |  }
    |}
    """.stripMargin)

    val registry = new Registry()
    registry.addHander("generalizePostalCode", Handlers.generalizePostalCode)
    registry.addHander("replaceWithPseudonym", Handlers.idReplacementHandler(new THS())) // this is just a dummy to ensure that the registration is working

    val deidentifhir = Deidentifhir(config, registry)

    val patient = new Patient()
    patient.setId("test")
    patient.setActive(true)
    patient.addAddress().setPostalCode("12345")

    val pPatient : Patient = deidentifhir.deidentify(patient).asInstanceOf[Patient]

    assert(pPatient.getActive)
    assert(pPatient.getAddressFirstRep.getPostalCode.equals("123"))
    assert(pPatient.getId.equals("dummy"))

    // TODO add assertions
  }
}
