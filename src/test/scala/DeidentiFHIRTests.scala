import ca.uhn.fhir.context.FhirContext
import de.stereotypez.deidentifhir.{Builder, Profile}
import org.hl7.fhir.instance.model.api.IBaseResource
import org.hl7.fhir.r4.model.{Patient, Resource}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source

class DeidentiFHIRTests extends AnyWordSpec with Matchers {

  val fctx = FhirContext.forR4()
  val parser = fctx.newJsonParser().setPrettyPrint(true)
  val deidentifhir = Builder(Profile.Test).build()

  val rIn = parser.parseResource[Patient](classOf[Patient], getClass.getResourceAsStream("/patient-example.json"))

  val rOut: Resource = deidentifhir.deidentify(rIn)

  print(parser.encodeResourceToString(rOut))

  //Source.fromResource("src/test/resources/patient-example.json").mkString

}
