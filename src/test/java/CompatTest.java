import de.stereotypez.deidentifhir.Builder;
import de.stereotypez.deidentifhir.Deidentifhir;
import de.stereotypez.deidentifhir.Profile;
import de.stereotypez.deidentifhir.util.Helpers;
import de.stereotypez.deidentifhir.util.JavaCompat;

public class CompatTest {

    public static void main(String[] args) {

        Deidentifhir deidentifier = new Builder(Profile.Test())
          .register("", JavaCompat.toScalaHandler((String[] strings, String s) -> Helpers.sha256(s)))
          .build();
    }
}
