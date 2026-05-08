package de.ume.deidentifhir.tools

import ca.uhn.fhir.context.FhirContext
import com.typesafe.config.ConfigFactory
import de.ume.deidentifhir.util._
import de.ume.deidentifhir.{Deidentifhir, FhirPath, ModuleBuilder, Registry}
import org.hl7.fhir.r4.model.Resource
import scopt.OParser

import java.io.{File, FileInputStream, PrintWriter}
import java.nio.file.Path

object ModuleConfigTester {

  val fctx = FhirContext.forR4()
  val parser = fctx.newJsonParser().setPrettyPrint(true)

  case class TestResult(file: File, success: Boolean, reason: Option[String])
  case class Arguments(moduleConfigFile: Path = null, exampleDirectory: Path = null, expectedDirectory: Path = null, tmpDirectory: Path = null)

  def main(args: Array[String]): Unit = {

    val builder = OParser.builder[Arguments]
    val oParser = {
      import builder._
      OParser.sequence(
        programName("ModuleConfigTester"),
        opt[File]("moduleConfigFile")
          .required()
          .validate(x =>
            if(x.isFile) success
            else failure("Must be a deidentiFHIR module config!"))
          .action((x,c) => c.copy(moduleConfigFile = x.toPath))
          .text("deidentiFHIR module configuration file"),
        opt[File]("exampleDirectory")
          .required()
          .validate(x =>
            if(x.isDirectory) success
            else failure("Must be a directory!"))
          .action((x,c) => c.copy(exampleDirectory = x.toPath))
          .text("Directory containing JSON encoded example resources."),
        opt[File]("expectedDirectory")
          .required()
          .validate(x =>
            if(x.isDirectory) success
            else failure("Must be a directory!"))
          .action((x,c) => c.copy(expectedDirectory = x.toPath))
          .text("Directory containing JSON encoded expected resources."),
        opt[File]("tmpDirectory")
          .required()
          .validate(x =>
            if(x.isDirectory) success
            else failure("Must be a directory!"))
          .action((x,c) => c.copy(tmpDirectory = x.toPath))
          .text("Directory used to store files temporarily. Will be deleted before each run!"),
      )
    }

    OParser.parse(oParser, args, Arguments()) match {
      case Some(arguments) => executeTests(arguments)
      case _ => // arguments are bad, error message will have been displayed
    }
  }

  private object DummyReplacementProvider extends IDReplacementProvider with IdentifierValueReplacementProvider {
    override def getIDReplacement(resourceType: String, id: String): String = Helpers.sha256(resourceType+id)

    override def getValueReplacement(system: String, value: String): String = Helpers.sha256(system+value)
  }

  def executeTests(arguments: Arguments): Unit = {
    val config = ConfigFactory.parseFile(arguments.moduleConfigFile.toFile)
    val registry = new Registry()
    // provide dummy implementations for all handlers
    registry.addHander("idReplacementHandler", Handlers.idReplacementHandler(DummyReplacementProvider))
    registry.addHander("referenceReplacementHandler", Handlers.referenceReplacementHandler(DummyReplacementProvider))
    registry.addHander("identifierValueReplacementHandler", Handlers.identifierValueReplacementHandler(DummyReplacementProvider, true))
    registry.addHander("generalizePostalCode", Handlers.generalizePostalCode)

    val module = ModuleBuilder(config, registry).build()
    val deidentifhir = new Deidentifhir(Seq(module))

    val examplesFiles = arguments.exampleDirectory.toFile
      .listFiles
      .filter(_.isFile)
      .toSeq

    // TODO clear the tmp directory

    val testResults = examplesFiles.map(exampleFile => {
      val expectedFile = arguments.expectedDirectory.resolve(exampleFile.getName).toFile
      testModuleWithExample(deidentifhir, module.pattern, exampleFile, expectedFile, arguments.tmpDirectory)
    })

    val (successfulTests, failedTests) = testResults.partition(_.success)
    println(s"${successfulTests.size} tests succeeded!")
    println(s"${failedTests.size} tests failed!")
    failedTests.foreach(failedTestResult => {
      println(s"Test for file ${failedTestResult.file.toPath.getFileName} failed for the following reason:\n\t${failedTestResult.reason.get}")
    })
  }

  def testModuleWithExample(deidentifhir: Deidentifhir, pattern: FhirPath, exampleFile: File, expectedFile: File, tmpDir: Path): TestResult = {

      val exampleResource = parser.parseResource(new FileInputStream(exampleFile)).asInstanceOf[Resource]

      // skip resources that do not match the module's pattern
      if(!DeidentifhirUtils.resourceMatchesFhirPath(exampleResource, pattern)) {
        return TestResult(exampleFile, false, Some("Given resource doesn't match the specified FHIRPath pattern!"))
      }

      val deidentifiedResource = deidentifhir.deidentify(exampleResource)

      if(expectedFile.exists()) {
        val expectedResource = parser.parseResource(new FileInputStream(expectedFile)).asInstanceOf[Resource]

        // TODO this check is fragile, since it depends on the implementation details that resources are printed in the same way all the time
        if(parser.encodeResourceToString(expectedResource).equals(parser.encodeResourceToString(deidentifiedResource))) {
          TestResult(exampleFile, true, None)
        } else {
          // write the actual deidentification result to the tmp folder
          val printer = new PrintWriter(tmpDir.resolve(expectedFile.getName).toFile)
          printer.println(parser.encodeResourceToString(deidentifiedResource))
          printer.flush()
          printer.close()

          TestResult(exampleFile, false, Some("The de-identified resource doesn't match the expected resource."))
        }
      } else {
        TestResult(exampleFile, false, Some(s"Unable to find file ${expectedFile.toString} that should contain the expected deidentification result."))
      }
  }
}
