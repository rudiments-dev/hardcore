package dev.rudiments.another

import io.circe.Json
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class ThingEncoderTest extends WordSpec with Matchers {

  case class Test(f : Option[Double]) extends DTO

  private val typeSystem = TypeSystem()
  typeSystem.makeFromScala[Thing, Thing]
  typeSystem.makeFromScala[Spec, Test]
  typeSystem.makeFromScala[Spec, AlgebraicExample]
  val encoder = new ThingEncoder(typeSystem)

  "encode class with option flag" in {
    val value = typeSystem.find[Spec]("Test").fromProduct(typeSystem, Test(None))
    encoder.specEncoder("Test")(value) should be (Json.obj("f" -> Json.Null))
  }

  "encode ADT" in {
    val valueB = typeSystem
      .find[Spec]("AlgebraicExample")
      .fromProduct(typeSystem, AlgebraicExample(B))
    encoder.specEncoder("AlgebraicExample")(valueB) should be (Json.obj(
      "a" -> Json.obj("type" -> Json.fromString("B"))
    ))

    val valueC = typeSystem
      .find[Spec]("AlgebraicExample")
      .fromProduct(typeSystem, AlgebraicExample(C("something algebraic")))
    encoder.specEncoder("AlgebraicExample")(valueC) should be (Json.obj(
      "a" -> Json.obj(
        "type" -> Json.fromString("C"),
        "s" -> Json.fromString("something algebraic")
      )
    ))
  }

  "encode Spec" ignore {
    val spec = typeSystem.find[Spec]("Spec")
    val value = spec.fromProduct(typeSystem, spec)
    encoder.specEncoder("Spec")(value) should be (Json.obj(

    ))
  }
}
