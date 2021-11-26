package de.stereotypez.deidentifhir

import ca.uhn.fhir.context.FhirContext
import com.typesafe.config.ConfigFactory
import de.stereotypez.deidentifhir.Deidentifhir.DeidentifhirHandler
import org.hl7.fhir.instance.model.api.IPrimitiveType
import org.hl7.fhir.r4.model.Bundle.BundleEntryComponent
import org.hl7.fhir.r4.model.Dosage.DosageDoseAndRateComponent
import org.hl7.fhir.r4.model.Narrative.NarrativeStatus
import org.hl7.fhir.r4.model._
import org.scalatest.funsuite._

import java.util
import scala.collection.mutable.ListBuffer

class DeidentiFHIRUnitTests extends AnyFunSuite {

  type MapOfPathHandlers = Map[String, Option[Seq[DeidentifhirHandler[Any]]]]
  type MapOfTypeHandlers = Map[Class[_], Option[Seq[DeidentifhirHandler[Any]]]]

  // this handler returns always null
  val nullHandler: DeidentifhirHandler[Base] = (_: Seq[String], _: Base, context: Seq[Base]) => {null}

  test("handler returns null") {

    val module = Module(
      pattern = ResourceExistsPath("Patient"),
      pathHandlers = Map("Patient.address.postalCode" -> Some(Seq(nullHandler))).asInstanceOf[MapOfPathHandlers],
      typeHandlers = Map())
    val deidentiFHIR = new Deidentifhir(Seq(module))

    val patient = new Patient()
    patient.addAddress(new Address().setPostalCode("4478"))
    val pPatient : Patient = deidentiFHIR.deidentify(patient).asInstanceOf[Patient]

    assert(pPatient.getAddress.size() == 0)
  }

  test("removal of empty elements") {
    val enc = new Encounter()
    val innerExt = new Extension()
    innerExt.setUrl("dummy")
    innerExt.setValue(new Coding().setSystem("nummy").setCode("dummy"))
    innerExt.setExtension(new util.LinkedList[Extension]()) // when resources are parsed using the FhirContext.forR4().newJsonParser(), an empty list might be added at this position
    val outerExt = new Extension()
    outerExt.setUrl("dummy")
    outerExt.addExtension(innerExt)

    enc.addExtension(outerExt)

    val deidentifhir = new Deidentifhir(Seq())
    val pEnc : Encounter = deidentifhir.deidentify(enc).asInstanceOf[Encounter]

    assert(pEnc.hasExtension === false)
    val jsonParser = FhirContext.forR4().newJsonParser().setPrettyPrint(true)
    assert(!jsonParser.encodeResourceToString(pEnc).contains("null"))
  }

  test("extensions in source resource are not altered") {

    val ext = new Extension()
    ext.setUrl("dummy")
    val value = new Coding().setSystem("dummy").setCode("dummy")
    ext.setValue(value)

    val address = new Address
    address.setUse(Address.AddressUse.HOME)
    address.addExtension(ext)

    val patient = new Patient()
    patient.addAddress(address)

    val deidentifhir = new Deidentifhir(Seq())
    val pPatient : Patient = deidentifhir.deidentify(patient).asInstanceOf[Patient]

    val extensions = patient.getAddressFirstRep.getExtension
    assert(extensions.size()==1)
    assert(extensions.get(0).getValue == value)
    assert(extensions.get(0).getUrl.equals("dummy"))

    assert(pPatient.hasAddress()==false)
  }

  test("extensions on primitive types are handled correctly") {

    val ext = new Extension()
    ext.setUrl("http://hl7.org/fhir/StructureDefinition/patient-birthTime")
    val value = new DateTimeType("1974-12-25T14:35:45-05:00")
    ext.setValue(value)

    val patient = new Patient()
    patient.getBirthDateElement.addExtension(ext)

    println(FhirContext.forR4().newJsonParser().setPrettyPrint(true).encodeResourceToString(patient))

    val config = ConfigFactory.parseString(
      """
      |    pattern = "Patient.exists()"
      |    base = [
      |      "Patient.birthDate.extension.url"
      |    ]
      |    paths = {
      |    }
      """.stripMargin)
    val module = ModuleBuilder(config).build()
    val deidentifhir = new Deidentifhir(Seq(module))

    val pPatient : Patient = deidentifhir.deidentify(patient).asInstanceOf[Patient]

    println(FhirContext.forR4().newJsonParser().setPrettyPrint(true).encodeResourceToString(pPatient))

    assert(pPatient.getBirthDateElement.hasExtension)
    assert(pPatient.getBirthDateElement.getExtensionFirstRep.hasUrl)
    assert(!pPatient.getBirthDateElement.getExtensionFirstRep.hasValue)
  }

  test("extensions on non-primitive types are handled correctly") {

    val ext = new Extension()
    ext.setUrl("dummy")
    val value = new Coding().setSystem("dummy").setCode("dummy")
    ext.setValue(value)

    val address = new Address
    address.setUse(Address.AddressUse.HOME)
    address.addExtension(ext)
    assert(!address.isInstanceOf[IPrimitiveType[_]])

    val patient = new Patient()
    patient.addAddress(address)

    val config = ConfigFactory.parseString(
      """
        |    pattern = "Patient.exists()"
        |    base = [
        |      "Patient.address.extension.url"
        |    ]
        |    paths = {
        |    }
      """.stripMargin)
    val module = ModuleBuilder(config).build()
    val deidentifhir = new Deidentifhir(Seq(module))

    val pPatient : Patient = deidentifhir.deidentify(patient).asInstanceOf[Patient]

    assert(pPatient.getAddressFirstRep.hasExtension)
    assert(pPatient.getAddressFirstRep.getExtensionFirstRep.hasUrl)
    assert(!pPatient.getAddressFirstRep.getExtensionFirstRep.hasValue)
  }

  test("maintain lists of items") {

    val config = ConfigFactory.parseString(
      """
        |deidentiFHIR.profile.version=0.2
        |modules = {
        |  module1: {
        |    pattern = "Patient.exists()"
        |    base = [
        |      "Patient.address.postalCode"
        |    ]
        |    paths = {
        |    }
        |  }
        |}
      """.stripMargin)
    val deidentifhir = Deidentifhir(config)

    val patientWithTwoAdresses = new Patient()
    patientWithTwoAdresses.addAddress(new Address().setPostalCode("44789"))
    patientWithTwoAdresses.addAddress(new Address().setPostalCode("45678"))
    val pPatient : Patient = deidentifhir.deidentify(patientWithTwoAdresses).asInstanceOf[Patient]
    assert(pPatient.getAddress.size() == 2)
  }

  // if Patient.meta.profile will be of size 1 but all elemnts are null, serialization will fail
  test("remove empty lists") {
    val deidentifhir = new Deidentifhir(Seq())

    val patient = new Patient()
    patient.setMeta(new Meta().addProfile("dummyProfileURL"))
    val pPatient : Patient = deidentifhir.deidentify(patient).asInstanceOf[Patient]

    val fctx = FhirContext.forR4()
    val parser = fctx.newJsonParser().setPrettyPrint(true)
    println(parser.encodeResourceToString(pPatient))
  }

  test("ChoiseOfType handling") {
    val ms = new MedicationStatement().addDosage(new Dosage().addDoseAndRate(new DosageDoseAndRateComponent().setDose(new SimpleQuantity().setValue(1).setUnit("Beutel").setSystem("test"))))
    val deidentifhir = new Deidentifhir(Seq())
    val rOut: Resource = deidentifhir.deidentify(ms.asInstanceOf[Resource])
  }

  test("ChoiseOfType handling for primitive types") {

    val config = ConfigFactory.parseString(
      """
        |deidentiFHIR.profile.version=0.2
        |modules = {
        |  module1: {
        |    pattern = "Observation.exists()"
        |    base = [
        |      "Observation.value[string]"
        |    ]
        |    paths = {
        |    }
        |  }
        |}
      """.stripMargin)

    val deidentifhir = Deidentifhir(config)

    val obs = new Observation()
    obs.setValue(new StringType("stringValue"))
    val pObs : Observation = deidentifhir.deidentify(obs).asInstanceOf[Observation]
    assert(pObs.hasValue)
    assert(pObs.hasValueStringType)
  }

  test("ChoiseOfType handling for complex types") {

    val config = ConfigFactory.parseString(
      """
        |deidentiFHIR.profile.version=0.2
        |modules = {
        |  module1: {
        |    pattern = "MedicationAdministration.exists()"
        |    base = [
        |      "MedicationAdministration.medication[Reference].reference"
        |    ]
        |    paths = {
        |    }
        |  }
        |}
      """.stripMargin)

    val deidentifhir = Deidentifhir(config)

    val ma = new MedicationAdministration()
    ma.setMedication(new Reference("test"))
    val pMa : MedicationAdministration = deidentifhir.deidentify(ma).asInstanceOf[MedicationAdministration]
    assert(pMa.hasMedication)
    assert(pMa.hasMedicationReference)
  }

  // When we register no handler for a FHIR path, it is expected that the element will not be changed by a call to
  // deidentify.
  test("register no handler to path") {

    val config = ConfigFactory.parseString(
      """
      |deidentiFHIR.profile.version=0.2
      |modules = {
      |  module1: {
      |    pattern = "Patient.exists()"
      |    base = [
      |      "Patient.active"
      |    ]
      |    paths = {
      |    }
      |  }
      |}
      """.stripMargin)

    val deidentifhir = Deidentifhir(config)

    val patient = new Patient()
    patient.setActive(true)
    val pPatient : Patient = deidentifhir.deidentify(patient).asInstanceOf[Patient]
    assert(pPatient.hasActive)

    // check that we do not return the same instance
    assert(patient.getActiveElement ne pPatient.getActiveElement)
  }

  test("handler application order") {

    val executionOrder = new ListBuffer[String]()

    val firstPathHandler: DeidentifhirHandler[Base] = (_: Seq[String], base: Base, context: Seq[Base]) => {executionOrder.append("first"); base}
    val secondPathHandler: DeidentifhirHandler[Base] = (_: Seq[String], base: Base, context: Seq[Base]) => {executionOrder.append("second"); base}
    val typeHandler: DeidentifhirHandler[BooleanType] = (_: Seq[String], base: BooleanType, context: Seq[Base]) => {executionOrder.append("type"); base}

    val module = Module(
      pattern = ResourceExistsPath("Patient"),
      pathHandlers = Map("Patient.active" -> Some(Seq(firstPathHandler, secondPathHandler))).asInstanceOf[MapOfPathHandlers],
      typeHandlers = Map(classOf[BooleanType] -> Some(Seq(typeHandler))).asInstanceOf[MapOfTypeHandlers])

    val deidentiFHIR = new Deidentifhir(Seq(module))

    val patient = new Patient()
    patient.setActive(true)
    deidentiFHIR.deidentify(patient).asInstanceOf[Patient]

    assert(executionOrder.toSeq === Seq("type", "first", "second"))
  }

  test("versionId is removed when id is handled") {
    val initial = new DiagnosticReport().setMeta(new Meta().setVersionId("versionnn")).setId("test")
    val parser = FhirContext.forR4().newJsonParser().setPrettyPrint(true)
    val diagnosticReport = parser.parseResource(parser.encodeResourceToString(initial)).asInstanceOf[Resource]

    val idTypeHandler: DeidentifhirHandler[IdType] = (_: Seq[String], _: IdType, _: Seq[Base]) => {
      new IdType("pseudo")
    }

    val module = Module(
      pattern = ResourceExistsPath("DiagnosticReport"),
      pathHandlers = Map("DiagnosticReport.id" -> Some(Seq(idTypeHandler))).asInstanceOf[MapOfPathHandlers],
      typeHandlers = Map())
    val deidentiFHIR = new Deidentifhir(Seq(module))

    val deidentifiedReport = deidentiFHIR.deidentify(diagnosticReport)
    assert(deidentifiedReport.getMeta.getVersionId == null)
  }

  test("nested extension can have no value") {

    val inner = new Extension()
    inner.setUrl("dummy")
    val outer = new Extension()
    outer.setUrl("dummy")
    outer.addExtension(inner)
    val encounter = new Encounter()
    encounter.addExtension(outer)

    val module = Module(pattern = ResourceExistsPath("Encounter"), Map(), Map())
    val deidentiFHIR = new Deidentifhir(Seq(module))

    val deidentifiedRes = deidentiFHIR.deidentify(encounter)
  }

  test("context is building correctly") {

    val patient = new Patient()
    val address = new Address().setPostalCode("44789");
    patient.addAddress(address)
    // TODO check if extensions are also included in the context correctly -> they are not!
//    val extension = new Extension()
//    extension.setUrl("dummy_url")
//    extension.setValue(new StringType("test"))
//    address.addExtension(extension)

    val assertionsForContextHandler = (_: Seq[String], base: Base, context: Seq[Base]) => {
      assert(context.size==2)
      assert(context(0) == patient)
      assert(context(1) == address)
      base
    }

    val registry = new Registry()
    registry.addHander("assertionsForContextHandler", assertionsForContextHandler)

    val config = ConfigFactory.parseString(
      """
        |deidentiFHIR.profile.version=0.2
        |modules = {
        |  module1: {
        |    pattern = "Patient.exists()"
        |    base = [
        |      "Patient.address.postalCode"
        |    ]
        |    paths = {
        |       "Patient.address.postalCode" : { handler = assertionsForContextHandler }
        |    }
        |  }
        |}
    """.stripMargin)

    val deidentifhir = Deidentifhir(config, registry)

    val pPatient : Patient = deidentifhir.deidentify(patient).asInstanceOf[Patient]
  }

  val fctx = FhirContext.forR4()
  val parser = fctx.newJsonParser().setPrettyPrint(true)

  ignore("patient with text with status and div") {

    val narrative = new Narrative().setStatus(NarrativeStatus.GENERATED)
    narrative.setDivAsString("test")
    val inputBundle = new Patient().setText(narrative)

    val outputBundle = Deidentifhir.buildKeepAll()._1.deidentify(inputBundle).asInstanceOf[Patient]

    assert(parser.encodeResourceToString(inputBundle) == parser.encodeResourceToString(outputBundle))
  }

  test("bundle with single resource") {
    val inputBundle = new Bundle().addEntry(new BundleEntryComponent().setResource(new Patient().setActive(true)));

    val outputBundle = Deidentifhir.buildKeepAll()._1.deidentify(inputBundle).asInstanceOf[Bundle]

    assert(parser.encodeResourceToString(inputBundle) == parser.encodeResourceToString(outputBundle))
  }

  // TODO split into a builder and a deidentifhir test
  test("config with two modules") {
    val config = ConfigFactory.parseString(
    """
    |deidentiFHIR.profile.version=0.2
    |modules = {
    |  module1: {
    |    pattern = "Observation.meta.profile contains 'profile1'"
    |    base = [
    |      "Observation.id"
    |    ]
    |    paths = {
    |    }
    |  }
    |  module2: {
    |    pattern = "Observation.meta.profile contains 'profile2'"
    |    base = [
    |      "Observation.status"
    |    ]
    |    paths = {
    |    }
    |  }
    |}
    """.stripMargin)

    val deidentiFHIR = Deidentifhir(config)

    // observation with profile matching the first module
    val obs1 = new Observation()
    obs1.setId("1")
    obs1.setStatus(Observation.ObservationStatus.FINAL)
    obs1.getMeta.addProfile("profile1")
    val deidentified_obs1 = deidentiFHIR.deidentify(obs1).asInstanceOf[Observation]
    assert(deidentified_obs1.hasId)
    assert(!deidentified_obs1.hasStatus)

    // observation with profile matching the second module
    val obs2 = new Observation()
    obs2.setId("2")
    obs2.setStatus(Observation.ObservationStatus.FINAL)
    obs2.getMeta.addProfile("profile2")
    val deidentified_obs2 = deidentiFHIR.deidentify(obs2).asInstanceOf[Observation]
    assert(!deidentified_obs2.hasId)
    assert(deidentified_obs2.hasStatus)

    // observation with profiles matching both modules
    val obs3 = new Observation()
    obs3.setId("3")
    obs3.setStatus(Observation.ObservationStatus.FINAL)
    obs3.getMeta.addProfile("profile1")
    obs3.getMeta.addProfile("profile2")
    val deidentified_obs3 = deidentiFHIR.deidentify(obs3).asInstanceOf[Observation]
    assert(deidentified_obs3.hasId)
    assert(deidentified_obs3.hasStatus)

    // observation with no profiles
    val obs4 = new Observation()
    obs4.setId("4")
    obs4.setStatus(Observation.ObservationStatus.FINAL)
    val deidentified_obs4 = deidentiFHIR.deidentify(obs4).asInstanceOf[Observation]
    assert(deidentified_obs4==null)
  }
}
