//public class CompatTest {
//
//    public static void main(String[] args) {
//
//        FhirContext fctx = FhirContext.forR4();
//        IParser parser = fctx.newJsonParser().setPrettyPrint(true);
//
//        // load the example
//        InputStream is = CompatTest.class.getClassLoader().getResourceAsStream("patient-example.in.json");
//        IBaseResource r = parser.parseResource(is);
//
//        Deidentifhir deidentifier = Builder.apply()
//          .register("Patient.id", JavaCompat.toScalaHandler((String[] path, IdType id, resource: Resource) -> {
//              //id.setIdBase(Helpers.sha256(id.getIdBase()));
//              return id;
//          }))
//          .build();
//
//        Patient pPatient = (Patient)deidentifier.deidentify((Resource) r);
//        System.out.println(pPatient.getIdBase());
//    }
//}
