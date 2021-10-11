package de.stereotypez.deidentifhir

import com.typesafe.config.ConfigFactory
import de.stereotypez.deidentifhir.Deidentifhir.DeidentifhirHandler
import de.stereotypez.deidentifhir.util.Handlers.generalizePostalCodeHandler
import org.hl7.fhir.r4.model.Base
import org.scalatest.funsuite.AnyFunSuite

import java.io.File

class ModuleBuilderTests extends AnyFunSuite {

  // TODO check if a path is valid
  // TODO add a test with a config without any paths upfront

  test("module config") {
    val config = ConfigFactory.parseString(
      """
        |  pattern = "Observation.meta.profile contains 'https://www.medizininformatik-initiative.de/fhir/core/modul-labor/StructureDefinition/ObservationLab'"
        |  base = [
        |    # Register a path for unmodified keeping"
        |    "Observation.id"
        |  ]
        |  paths = {
        |    # Register a handler without any parameters
        |    "*.id" : { handler = replaceWithPseudonym }
        |  }
    """.stripMargin)

    val module = ModuleBuilder(config).build()

    assert(module.pattern.equals(ProfileFhirPath("Observation", "https://www.medizininformatik-initiative.de/fhir/core/modul-labor/StructureDefinition/ObservationLab")))
    assert(module.pathHandlers.keySet.size==1)
  }

  test("programmatically register multiple handlers for the same path") {
    val config = ConfigFactory.parseString(
      """
      |pattern = "Observation.meta.profile contains 'https://www.medizininformatik-initiative.de/fhir/core/modul-labor/StructureDefinition/ObservationLab'"
      |base = [
      |  "Observation.id"
      |]
      |paths = {
      |}
      """.stripMargin
    )

    val module = ModuleBuilder(config)
      .register("Observation.id", None)
      .register("Observation.id", None)
      .register("Observation.id", generalizePostalCodeHandler) // this could be any handler
      .register("Observation.id", None)
      .register("Observation.id", generalizePostalCodeHandler) // this could be any handler
      .build()
    // Nones should be ignored in this case, as they do not have any influence on the outcome.
    val handlers = module.pathHandlers.get("Observation.id")
    assert(handlers.isDefined && handlers.get.isDefined && handlers.get.get.size==2)
  }

  val dummyHandler: Option[DeidentifhirHandler[Base]] = Some((path: Seq[String], base: Base, context: Seq[Base]) => base)

  test("load profile with non existing FHIR paths") {
    val config = ConfigFactory.parseString(
      """
        |  pattern = "Patient.exists()"
        |  base = [
        |    "Patient.doesNotExist"
        |  ]
        |  paths = {}
      """.stripMargin)

    ModuleBuilder(config).build()

    // TODO it is currently possible to load Modules with non existing FHIR paths. This needs to throw an error!
    fail()
  }

  test("register handler for non existent FHIR path") {

    val config = ConfigFactory.parseString(
      """
        |  pattern = "Patient.exists()"
        |  base = [
        |  ]
        |  paths = {
        |  }
      """.stripMargin)

    val thrown = intercept[Exception] {
      ModuleBuilder(config)
        .register("Patient.doesNotExist", dummyHandler)
        .build()
    }
    assert(thrown.getMessage === "trying to register a handler to the unspecified FHIR path Patient.doesNotExist!")
  }
}
