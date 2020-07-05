package dev.rudiments.hardcore.types

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable.ListMap

@RunWith(classOf[JUnitRunner])
class AlgebraicSpec extends WordSpec with Matchers {
  sealed trait A extends ADT {
    case object B extends A
    case class C(s: String) extends A
  }

  case class AlgebraicExample(a: A) extends DTO

  private implicit val typeSystem: TypeSystem = new TypeSystem()
  private val scalaType: Type = ScalaType[AlgebraicExample]

  private val objectB = Singleton("B")
  private val typeC = Type("C", ListMap("s" -> Field(ScalaTypes.ScalaString, FieldFlag.Required)))
  private val typeA = Algebraic("A", Declaration("A"), Seq(objectB, typeC))

  private implicit val prototype: Type = Type(
    "AlgebraicExample",
    ListMap("a" -> Field(Types.Reference(typeA), FieldFlag.Required))
  )

  "Can create instances with different content" in {
    prototype.construct(objectB) should be (SoftInstance(Map("a" -> objectB)))

    val c = SoftInstance("string inside ADT:C")(typeC)
    prototype.construct(c) should be (SoftInstance(Map("a" -> c)))
  }

  "Algebraic field from Scala Type" in {
    scalaType.name should be (prototype.name)
    scalaType.fields should be (prototype.fields)
  }
}
