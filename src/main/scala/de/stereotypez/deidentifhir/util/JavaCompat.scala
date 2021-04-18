package de.stereotypez.deidentifhir.util

import de.stereotypez.deidentifhir.Deidentifhir.DeidentifhirHandler

import java.util.function.BiFunction

object JavaCompat {

  def toScalaHandler[T](fun: BiFunction[Array[String], T, T]): Option[DeidentifhirHandler[T]] = {
    Some((path: Seq[String], t: T) => fun.apply(path.toArray, t))
  }

}
