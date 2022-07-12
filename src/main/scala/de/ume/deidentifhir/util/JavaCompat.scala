package de.ume.deidentifhir.util

import de.ume.deidentifhir.Deidentifhir.DeidentifhirHandler
import org.hl7.fhir.r4.model.{Base, Resource}

import java.util.function.BiFunction

object JavaCompat {

  // This helper method comes handy if you want to define a handler in Java.
  def toScalaHandler[T](fun: BiFunction[Array[String], T, T]): Option[DeidentifhirHandler[T]] = {
    Some((path: Seq[String], t: T, context: Seq[Base], staticContext: Map[String, String]) => fun.apply(path.toArray, t))
  }

  //
  def partiallyApply[K,T](provider: K, passedFunction: (K, Seq[String], T, Seq[Base], Map[String, String]) => T): DeidentifhirHandler[T] = {
    passedFunction.apply(provider, _, _, _, _)
  }

  def partiallyApply2[K,B,T](provider: K, value: B, passedFunction: (K, B, Seq[String], T, Seq[Base], Map[String, String]) => T): DeidentifhirHandler[T] = {
    passedFunction.apply(provider, value, _, _, _, _)
  }
}
