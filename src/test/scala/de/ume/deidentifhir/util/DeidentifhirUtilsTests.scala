package de.ume.deidentifhir.util

import de.ume.deidentifhir.Deidentifhir.DeidentifhirHandler
import de.ume.deidentifhir.IdentifierSystemFhirPath
import de.ume.deidentifhir.util.DeidentifhirUtils.{mergeHandlers, resourceMatchesFhirPath}
import org.hl7.fhir.r4.model.{Base, Identifier, Patient}
import org.scalatest.funsuite.AnyFunSuite

class DeidentifhirUtilsTests extends AnyFunSuite {

  test("merge path handlers from multiple modules") {

    val h1: DeidentifhirHandler[Any] = (_: Seq[String], base: Any, _: Seq[Base]) => base
    val h2: DeidentifhirHandler[Any] = (_: Seq[String], base: Any, _: Seq[Base]) => base
    val h3: DeidentifhirHandler[Any] = (_: Seq[String], base: Any, _: Seq[Base]) => base
    val h4: DeidentifhirHandler[Any] = (_: Seq[String], base: Any, _: Seq[Base]) => base

    val handlersFromModule1 = Some(Some(Seq(h1,h2))) // the module has multiple handler that should be applied
    val handlersFromModule2 = Some(Some(Seq(h3,h4)))
    val handlersFromModule3 = None  // there is a module, but it hasn't any applicable handlers for the given path
    val handlersFromModule4= Some(None) //the module has an applicable handler that indicates, that this path should be keep unmodified

    val test0 = mergeHandlers(Seq())
    assert(test0.equals(None))
    val test1 = mergeHandlers(Seq(handlersFromModule3, handlersFromModule4))
    assert(test1.equals(Some(None)))
    val test2 = mergeHandlers(Seq(handlersFromModule3, handlersFromModule3))
    assert(test2.equals(None))
    val test3 = mergeHandlers(Seq(handlersFromModule1, handlersFromModule2))
    assert(test3.equals(Some(Some(Seq(h1,h2,h3,h4)))))
    val test4 = mergeHandlers(Seq(handlersFromModule1, handlersFromModule3, handlersFromModule4, handlersFromModule2))
    assert(test4.equals(Some(Some(Seq(h1,h2,h3,h4)))))
  }

  test("resourceMatchesFhirPath") {

    val p = new Patient().addIdentifier(new Identifier().setSystem("testSystem"))
    assert(resourceMatchesFhirPath(p, IdentifierSystemFhirPath("Patient", "testSystem")))

    assert(!resourceMatchesFhirPath(new Patient(), IdentifierSystemFhirPath("Patient", "testSystem")))

    val q = new Patient().addIdentifier(new Identifier().setSystem("testSystem2"))
    assert(!resourceMatchesFhirPath(q, IdentifierSystemFhirPath("Patient", "testSystem")))

    val r = new Patient().addIdentifier(new Identifier().setSystem("testSystem2")).addIdentifier(new Identifier().setSystem("testSystem"))
    assert(resourceMatchesFhirPath(r, IdentifierSystemFhirPath("Patient", "testSystem")))
  }
}
