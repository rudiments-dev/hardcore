package dev.rudiments.hardcore.types

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class TypeSystemSpec extends WordSpec with Matchers {

  sealed trait A extends ADT {
    case object B extends A
    case class C(f: F) extends A
  }

  sealed trait F extends ADT {
    case class E(s: String) extends F
    case class D(a: A) extends F
  }

  case class SomethingA(a: A)
  case class SomethingF(f: F)

  private implicit val typeSystem: TypeSystem = new TypeSystem()

  "can save cross-dependent ADT into type system" in {
    val t = ScalaType[SomethingA]
    t.name should be ("SomethingA")

    val f = ScalaType[SomethingF]
    f.name should be ("SomethingF")

    typeSystem.types.foreach { case (name, thing) => println(name + ": " + thing.toString)}
  }
}