import ca.uhn.fhir.context.FhirContext
import de.stereotypez.deidentifhir.{Builder, Profile}
import org.hl7.fhir.instance.model.api.IBaseResource
import org.hl7.fhir.r4.model.{Patient, Resource}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.reflections.Reflections
import org.reflections.scanners.ResourcesScanner
import java.util.regex.Pattern
import scala.jdk.CollectionConverters._
import scala.io.Source
import scala.util.Using

class DeidentiFHIRTests extends AnyWordSpec with Matchers {

  val fctx = FhirContext.forR4()
  val parser = fctx.newJsonParser().setPrettyPrint(true)
  val deidentifhir = Builder(Profile.Test).build()

  val reflections = new Reflections("", new ResourcesScanner())

  reflections.getResources(Pattern.compile(".*\\.in\\.json")).asScala
    .foreach {
      case s"$name.in.json" =>

        val in = Using.resource(Source.fromFile(getClass.getResource(s"/$name.in.json").toURI)) { src =>
          src.getLines().mkString("\n")
        }
        val out = Using.resource(Source.fromFile(getClass.getResource(s"/$name.out.json").toURI)) { src =>
          src.getLines().mkString("\n")
        }

        val rIn = parser.parseResource[Patient](classOf[Patient], in)
        val rOut: Resource = deidentifhir.deidentify(rIn)

        println(parser.encodeResourceToString(rOut))

        parser.encodeResourceToString(rOut) shouldEqual out
    }

}
