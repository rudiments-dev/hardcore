package dev.rudiments.types

import dev.rudiments.types
import dev.rudiments.types.hard.{ScalaType, ScalaTypes}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable.ListMap

@RunWith(classOf[JUnitRunner])
class AlgebraicSpec extends WordSpec with Matchers {
  private implicit val typeSystem: TypeSystem = new TypeSystem()
  private val scalaType: ScalaType[AlgebraicExample] = typeSystem.asType[AlgebraicExample]

  private val typeA = Abstract("A")
  private val objectB = OnlyOne("B", Seq(typeA))
  private val typeC = Type("C", ListMap("s" -> Field(ScalaTypes.ScalaString, true)), Seq(typeA))

  private implicit val prototype: Type = Type(
    "AlgebraicExample",
    ListMap("a" -> Field(Algebraic(typeA, Set(objectB, typeC)), true))
  )

  "Can create instances with different content" in {
    prototype.construct(objectB) should be (Instance(Map("a" -> objectB)))

    val c = Instance("string inside ADT:C")(typeC)
    prototype.construct(c) should be (Instance(Map("a" -> c)))
  }

  "Algebraic field from Scala Type" in {
    scalaType.name should be (prototype.name)
    scalaType.fields should be (prototype.fields)

    scalaType.constructScala(B) should be (AlgebraicExample(B))
    scalaType.fromScala(AlgebraicExample(B)) should be (Instance(Map("a" -> objectB)))

    val c = C("From scala")
    scalaType.constructScala(c) should be (AlgebraicExample(c))
    scalaType.fromScala(AlgebraicExample(c)) should be (Instance(Map("a" -> typeC.fromScala(c))))
  }
}

sealed trait A extends ADT
case object B extends A
case class C(s: String) extends A

case class AlgebraicExample(a: A) extends DTO
