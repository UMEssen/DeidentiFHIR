package de.stereotypez.deidentifhir

import de.stereotypez.deidentifhir.Deidentifhir.DeidentifhirHandler

import scala.collection.mutable

class Registry() {

  // TODO create a RegistryBuilder instead of allowing to mutate the internal state of this class!
  private val handlers = new mutable.HashMap[String, DeidentifhirHandler[_]]()

  def addHander(name: String, handler: DeidentifhirHandler[_]): Unit = {
    if(handlers.contains(name))
      throw new IllegalStateException(s"There is already handler named $name registered!")

    handlers.put(name, handler)
  }

  def getHandler(name: String): Option[DeidentifhirHandler[_]] = {
    handlers.get(name)
  }

  def clear(): Unit = {
    handlers.clear()
  }
}
