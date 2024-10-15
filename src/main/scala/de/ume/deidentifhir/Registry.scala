package de.ume.deidentifhir

import de.ume.deidentifhir.Deidentifhir.DeidentifhirHandler

import scala.collection.mutable

class Registry() {

  // TODO create a RegistryBuilder instead of allowing to mutate the internal state of this class!
  private val handlers = new mutable.HashMap[String, DeidentifhirHandler[_]]()

  @deprecated("Use addHandler instead. This method will be removed in the next release.", "v0.2.11")
  def addHander(name: String, handler: DeidentifhirHandler[_]): Unit = {
    addHandler(name, handler)  // Delegate to the new method
  }

  def addHandler(name: String, handler: DeidentifhirHandler[_]): Unit = {
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
