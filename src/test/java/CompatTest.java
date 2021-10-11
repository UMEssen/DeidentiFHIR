import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.parser.IParser;
import de.stereotypez.deidentifhir.Builder;
import de.stereotypez.deidentifhir.Deidentifhir;
import de.stereotypez.deidentifhir.util.JavaCompat;
import org.hl7.fhir.instance.model.api.IBaseResource;
import org.hl7.fhir.r4.model.IdType;
import org.hl7.fhir.r4.model.Patient;
import org.hl7.fhir.r4.model.Resource;

import java.io.InputStream;

public class CompatTest {

    public static void main(String[] args) {

        FhirContext fctx = FhirContext.forR4();
        IParser parser = fctx.newJsonParser().setPrettyPrint(true);

        // load the example
        InputStream is = CompatTest.class.getClassLoader().getResourceAsStream("patient-example.in.json");
        IBaseResource r = parser.parseResource(is);

        Deidentifhir deidentifier = Builder.apply()
          .register("Patient.id", JavaCompat.toScalaHandler((String[] path, IdType id) -> {
              //id.setIdBase(Helpers.sha256(id.getIdBase()));
              return id;
          }))
          .build();

        Patient pPatient = (Patient)deidentifier.deidentify((Resource) r);
        System.out.println(pPatient.getIdBase());
    }
}
