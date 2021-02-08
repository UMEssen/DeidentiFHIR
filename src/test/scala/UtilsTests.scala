import de.stereotypez.deidentifhir.DeidentifhirUtils
import org.hl7.fhir.r4.model.{HumanName, ImagingStudy, Period}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.util.Date

class UtilsTests extends AnyWordSpec with Matchers {
new ImagingStudy()
  val hn = new HumanName()
  hn.addPrefix("Dr.")
  hn.addGiven("John")
  hn.addGiven("Alexander")
  hn.setFamily("Doe")
  hn.addSuffix("Jr.")
  hn.setUse(HumanName.NameUse.OFFICIAL)
  hn.setText("Lorem ipsum")
  hn.setPeriod(new Period().setStart(new Date(new Date().getTime - 123456)).setEnd(new Date()))

  DeidentifhirUtils.getChildrenWithValue(hn).map { case (fp, value) =>
    println(s"${fp.property.getName} [${fp.field.getType}]: $value")
  }

}
