package de.stereotypez.deidentifhir.util

import de.stereotypez.deidentifhir.Deidentifhir.DeidentifhirHandler
import org.hl7.fhir.r4.model.Base

import scala.collection.{immutable, mutable}

/**
 * This custom map implementation returns a handler for all paths. The handler logs the used path and returns the input value.
 */
class AlwaysReturnValueMap[A, Int] extends immutable.Map[String, Option[Seq[DeidentifhirHandler[Any]]]] {

  val log = new mutable.HashSet[String]()

  override def get(key: String): Option[Option[Seq[DeidentifhirHandler[Any]]]] = {
    Some(Some(Seq((path: Seq[String], any: Any, context: Seq[Base]) => {
      //println(path)
      log.add(path.mkString("."))
      any
    })))
  }

  override def removed(key: String): Map[String, Option[Seq[DeidentifhirHandler[Any]]]] = ???

  override def updated[V1 >: Option[Seq[DeidentifhirHandler[Any]]]](key: String, value: V1): Map[String, V1] = ???

  override def iterator: Iterator[(String, Option[Seq[DeidentifhirHandler[Any]]])] = ???
}