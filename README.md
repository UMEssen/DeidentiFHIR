# DeidentiFHIR

[![Affiliated with RTG WisPerMed](https://img.shields.io/badge/Affiliated-RTG%202535%20WisPerMed-blue)](https://wispermed.org/)

DeidentiFHIR is a generic FHIR de-identification library based on HAPI FHIR. It can process complete FHIR R4 bundles or single FHIR R4 resources.
DeidentiFHIR iterates recursively over the given resources and checks for each element how it should be processed based on the specified configuration. 

## Usage

```scala
// 1. Load a custom DeidentiFHIR configuration.
val config = ConfigFactory.parseFile(new File("/path/to/DeidentiFHIR.conf"))

// 2. Create a registry to provide implementations of handlers that are used in the config above.
val registry = new Registry()
registry.addHander("generalizePostalCode", Handlers.generalizePostalCode)

// 3. Construct an instance of DeidentiFHIR based on the config above.
val deidentifhir = Deidentifhir(config, registry)

// 4. De-identify a bundle.
val pBundle : Bundle = deidentifhir.deidentify(bundle).asInstanceOf[Bundle]
```

Instead of a whole bundle, you could also pass single resources to the call of `deidentify`. The given resource must be an instance of a [HAPI FHIR resource](https://hapifhir.io/hapi-fhir/apidocs/hapi-fhir-structures-r4/org/hl7/fhir/r4/model/Resource.html). 

If a handler needs some additional input that should be provided to it during the call of `deidentify`, you can also pass some static context as a map: 

```scala

val staticContext = Map(Handlers.patientIdentifierKey->"1234567")

// 4. De-identify a bundle.
val pBundle : Bundle = deidentifhir.deidentify(bundle, staticContext).asInstanceOf[Bundle]
```

The given FHIR bundle is not modified, but instead all changes are performed on a new object.

## Configuration

DeidentiFHIR uses [HOCON](https://github.com/lightbend/config/blob/master/HOCON.md) as its configuration format. This allows to split the configuration in several files and include these later on. The configuration consists of a list of modules. As can be seen in this example:

```
deidentiFHIR.profile.version=0.2
modules = {
    laboratory: {include required("lab_modul.conf")}
    person: {include required("person_module.conf")}
}
```

The modules contain rules that describe how specific resources should be processed by DeidentiFHIR. The processing of a resources can be affected by rules defined in multiple modules.

Although not recommended, the whole configuration could also be defined in a single file.

### Modules

A module configuration must contain three elements:

* The pattern is used to decide if the rules defined in this module should be applied to a given resource or not.
* A list of base paths. All elements defined here are keep unmodified. All elements that are not listed will be removed during the de-identification process. 
* A list of paths with corresponding handler registrations, that implement specific logic that should be applied to the corresponding elements. Only registration on paths that are included in the base paths are allowed.
* A list of types with corresponding handler registrations, that implement specific logic that should be applied to the corresponding elements. DeidentiFHIR is _not_ aware of any type hierarchies!

Example:

```
pattern = "Observation.meta.profile contains 'https://www.medizininformatik-initiative.de/fhir/core/modul-labor/StructureDefinition/ObservationLab'"
base = [
    "Observation.id",
    "Observation.value[Quantity].code",
    "Observation.value[Quantity].system",
    "Observation.value[Quantity].unit",
    "Observation.value[Quantity].value"
]
paths = {
    "Observation.id" : { handler = idReplacementHandler }
}
types = {
    "DateType" : { handler = dateShiftHandler }
}
```

### Supported Patterns

Patterns are based on [FHIRPath](https://hl7.org/fhirpath/). However, currently only very specific patterns are supported.

| Purpose                        | Used regular expression             | Example                                                                                                                                  |
|--------------------------------|-------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------|
| match a specific profile       | `(.*).meta.profile contains '(.*)'` | Observation.meta.profile contains 'https://www.medizininformatik-initiative.de/fhir/core/modul-labor/StructureDefinition/ObservationLab' |
| match a identifier system      | `(.*).identifier.system contains '(.*)'` | Patient.identifier.system contains 'https://www.example.org/fhir/sid/patienten' |
| match a specific resource type | `(.*).exists\\(\\)`                 | Observation.exists()                                                                                                                     |
| ...                            |                                     |                                                                                                                                          |

### Implemented Handlers

| name                              | expected HAPI FHIR type | functionality                                                                                                                                        |
|-----------------------------------|-------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------|
| identifierValueReplacementHandler | StringType              |                                                                                                                                                      |
| idReplacementHandler              | IdType                  |                                                                                                                                                      |
| referenceReplacementHandler       | StringType              |                                                                                                                                                      |
| generalizePostalCode              | StringType              | Truncates the postal code to the first three digits, if the given postal code is five digits long. Otherwise, the postal code is removed altogether. |
| generalizeDateHandler             | DateType                | Set the day of the month to the 15th according to the MII/SMITH pseudonymization concept.                                                            |
| shiftDateHandler                  | BaseDateTimeType        | Shift the DateType, DateTimeType or InstantType by given milliseconds (positive or negative value).                                                  |
| stringReplacementHandler          | StringType              | Replaces the given string with a predefined static string.                                                                                           |
| ...                               |                         |                                                                                                                                                      |                          |                                                                                                                                                      |
### Add custom handlers

Implementation of custom handlers is quite easy. You just need to define a function with the following signature:

```scala
type DeidentifhirHandler[T <: Any] = (Seq[String], T, Seq[Base], Map[String, String]) => T
```

Each handler gets passed
1. the path to the current element,
2. a copy (to avoid accidental mutation of the input) of the current element that should be processed 
3. and a sequence of all elements from the root to the current element.
4. a map of strings that is static for each call of deidentify and can store a static context for the current de-identification run
