package de.ume.deidentifhir.tools

import ca.uhn.fhir.context.FhirContext
import com.typesafe.config.{ConfigFactory, ConfigList, ConfigRenderOptions, ConfigValueFactory}
import de.ume.deidentifhir.util.DeidentifhirUtils
import de.ume.deidentifhir.util.DeidentifhirUtils.determinePattern
import de.ume.deidentifhir.{Deidentifhir, FhirPath}
import org.hl7.fhir.r4.model.{Bundle, Resource}
import scopt.OParser

import java.io.{File, FileInputStream, PrintWriter}
import java.nio.file.Path
import java.util
import scala.collection.JavaConverters._

object ModuleConfigGenerator {

  val fctx = FhirContext.forR4()
  val parser = fctx.newJsonParser().setPrettyPrint(true)

  case class Config(inputDirectory: Path = null, pattern: String = "", outputFile: Path = null)

  /**
   * Generates a config from a list of examples.
   */
  def main(args: Array[String]): Unit = {

    val builder = OParser.builder[Config]
    val oParser = {
      import builder._
      OParser.sequence(
        programName("ModuleConfigGenerator"),
        opt[File]("inputDirectory")
          .required()
          .validate(x =>
            if(x.isDirectory) success
            else failure("Must be a directory!"))
          .action((x,c) => c.copy(inputDirectory = x.toPath))
          .text("Directory containing JSON encoded example resources."),
        opt[String]("pattern")
          .required()
          .action((x,c) => c.copy(pattern = x)),
        opt[File]("outputFile")
          .required()
          .action((x,c) => c.copy(outputFile = x.toPath)),
      )
    }

    // OParser.parse returns Option[Config]
    OParser.parse(oParser, args, Config()) match {
      case Some(config) => createConfig(config)
      case _ => // arguments are bad, error message will have been displayed
    }
  }

  def createConfig(config: Config): Unit = {
    // read example resources from a directory
    val exampleResources = config.inputDirectory.toFile.listFiles.filter(_.isFile).toSeq
      .map(file => parser.parseResource(new FileInputStream(file)))
      .map(_ match {
        case bundle: Bundle     => bundle.getEntry.asScala.toSeq.map(_.getResource)
        case resource: Resource => Seq(resource)
        case _                  => throw new Exception("unexpected type!")
      }).flatten

    val pattern: FhirPath = determinePattern(config.pattern)

    // filter for resources that do not match the specified pattern
    val (validExampleResources, invalidExampleResources) = exampleResources.partition(DeidentifhirUtils.resourceMatchesFhirPath(_, pattern))
    println(s"Ignoring ${invalidExampleResources.size} resources, since they doesn't match the specified pattern!.")
    println(s"Using ${validExampleResources.size} resources to generate the initial module configuration.")

    val baseConfig = determineBaseConfigFromExamples(validExampleResources)
    writeConfigToFile(
      config.outputFile,
      config.pattern,
      baseConfig)
  }

  def determineBaseConfigFromExamples(exampleResources: Seq[Resource]) = {
    val (deidentiFHIR, keepMap) = Deidentifhir.buildKeepAll()

    exampleResources.foreach(r => {
      val deidentifiedRes = deidentiFHIR.deidentify(r)
      assert(parser.encodeResourceToString(deidentifiedRes) == parser.encodeResourceToString(r))
    })

    ConfigValueFactory.fromIterable(keepMap.log.toSeq.sorted.asJava)
  }

  def writeConfigToFile(path: Path, pattern: String, baseList: ConfigList): Unit = {
    path.getParent.toFile.mkdirs()

    val config = ConfigFactory.empty()
      .withValue("pattern", ConfigValueFactory.fromAnyRef(pattern))
      .withValue("base", baseList)
      .withValue("paths", ConfigValueFactory.fromMap(new util.HashMap[String, String]()))

    val printer = new PrintWriter(path.toFile)
    printer.println(config.root().render(ConfigRenderOptions.defaults().setComments(false).setOriginComments(false)))
    printer.flush()
    printer.close()
  }
}
