package dev.rudiments.another

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable.ListMap

@RunWith(classOf[JUnitRunner])
class AlgebraicSpec extends WordSpec with Matchers {
  private implicit val typeSystem: TypeSystem = TypeSystem()
  private val scalaType: Spec = typeSystem.makeFromScala[Spec, AlgebraicExample]

  private val typeA = Abstract("A")
  private val objectB = The("B")
  private val typeC = Spec("C", ListMap("s" -> ValueSpec(ScalaTypes.ScalaString, true)))

  private implicit val prototype: Spec = Spec(
    "AlgebraicExample",
    ListMap("a" -> ValueSpec(typeA, true))
  )

  "Can create instances with different content" in {
    prototype.instantiate(typeSystem, objectB) should be (Instance(prototype, Seq(objectB)))

    val c = Instance(typeC, Seq("string inside ADT:C"))
    prototype.instantiate(typeSystem, c) should be (Instance(prototype, Seq(c)))
  }

  "Algebraic field from Scala Type" in {
    scalaType.name should be (prototype.name)
    scalaType.fields should be (prototype.fields)

    scalaType.toScala[AlgebraicExample](typeSystem, B) should be (AlgebraicExample(B))
    scalaType.fromProduct(typeSystem, AlgebraicExample(B)) should be (Instance(prototype, Seq(objectB)))

    val c = C("From scala")
    scalaType.toScala[AlgebraicExample](typeSystem, c) should be (AlgebraicExample(c))
    scalaType.fromProduct(typeSystem, AlgebraicExample(c)) should be (Instance(prototype, Seq(typeC.fromProduct(typeSystem, c))))
  }
}

sealed trait A extends ADT
case object B extends A
case class C(s: String) extends A

case class AlgebraicExample(a: A) extends DTO
