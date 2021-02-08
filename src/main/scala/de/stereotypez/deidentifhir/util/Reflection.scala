package de.stereotypez.deidentifhir.util

import Hapi.nameToField

import java.lang.reflect.Field
import scala.annotation.tailrec

object Reflection {

  @tailrec
  def getAccessibleField(clazz: Class[_], fieldName: String): Field =
    try {
      val field = clazz.getDeclaredField(nameToField(fieldName))
      field.setAccessible(true)
      field
    }
    catch {
      case e: NoSuchFieldException =>
        val superClass = clazz.getSuperclass
        if (superClass == null) throw e
        else Reflection.getAccessibleField(superClass, fieldName);
    }

}
