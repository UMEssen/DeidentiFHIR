# DeidentiFHIR
DeidentiFHIR is a generic FHIR de-identification library based on HAPI FHIR. It can process complete FHIR R4 bundles or single FHIR R4 resources.
DeidentiFHIR iterates recursively over the given resources and checks for each element how it should be processed based on the specified configuration. 

## Usage

```
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

While it is the general goal of this library to not modify the given FHIR bundle, this is currently not true for resources that contain FHIR extensions.

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
* A list of handler registrations for elements that should be handled in a specific way. Only registration on paths that are included in the base paths are allowed.

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
| ...                               |                         |                                                                                                                                                      |                          |                                                                                                                                                      |
### Add custom handlers

Implementation of custom handlers is quite easy. You just need to define a function with the following signature:

```
type DeidentifhirHandler[T <: Any] = (Seq[String], T, Seq[Base]) => T
```

Each handler gets passed
1. the path to the current element,
2. a copy (to avoid accidental mutation of the input) of the current element that should be processed 
3. and a sequence of all elements from the root to the current element.
