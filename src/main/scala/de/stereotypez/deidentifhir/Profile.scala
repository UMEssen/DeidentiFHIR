package de.stereotypez.deidentifhir

import java.net.URL

object Profile {
  val Full: URL = getClass.getResource("/profiles/full.profile")
  val Clinical: URL = getClass.getResource("/profiles/clinical.profile")
  val Baseline: URL = getClass.getResource("/profiles/baseline.profile")
  val Test: URL = getClass.getResource("/profiles/test.profile")
}
