package de.ume.deidentifhir.util

trait IDReplacementProvider {
  def getIDReplacement(resourceType: String, id: String) : String
}

trait IdentifierValueReplacementProvider {
  def getValueReplacement(system: String, value: String) : String
}

trait ShiftDateProvider {
  def getDateShiftingValueInMillis(): java.lang.Long
}