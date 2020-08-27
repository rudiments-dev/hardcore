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

  "encode Spec" in {
    val spec = typeSystem.find[Spec]("Spec")
    val value = spec.fromProduct(typeSystem, spec)

    val textJson: Json = Json.obj(
      "type" -> Json.fromString("Text"),
      "maxSize" -> Json.obj(
        "type" -> Json.fromString("Big"),
        "size" -> Json.fromInt(Int.MaxValue)
      )
    )

    encoder.specEncoder("Spec")(value) should be (Json.obj(
      "name" -> Json.fromString("Spec"),
      "fields" -> Json.obj(
        "name" -> Json.obj(
          "thing" -> textJson,
          "isRequired" -> Json.True
        ),
        "fields" -> Json.obj(
          "thing" -> Json.obj(
            "type" -> Json.fromString("Index"),
            "of" -> textJson,
            "over" -> Json.obj(
              "type" -> Json.fromString("Spec"),
              "name" -> Json.fromString("ValueSpec"),
              "fields" -> Json.obj(
                "thing" -> Json.obj(
                  "thing" -> Json.obj(
                    "type" -> Json.fromString("Abstract"),
                    "name" -> Json.fromString("Thing")
                  ),
                  "isRequired" -> Json.True
                ),
                "isRequired" -> Json.obj(
                  "thing" -> Json.obj(
                    "type" -> Json.fromString("Bool")
                  ),
                  "isRequired" -> Json.True
                )
              )
            )
          ),
          "isRequired" -> Json.True
        )
      )
    ))
  }

  "encode Abstract Thing" in {
    val spec = typeSystem.find[Spec]("Abstract")
    val t = typeSystem.find[Abstract]("Thing")
    val value = spec.fromProduct(typeSystem, t)

    encoder.specEncoder("Abstract")(value) should be (Json.obj(
      "name" -> Json.fromString("Thing")
    ))
  }

  "encode Plain" in {
    val spec = typeSystem.find[Spec]("The")
    val t = typeSystem.find[The]("Bool")
    val value = spec.fromProduct(typeSystem, t)

    encoder.specEncoder("The")(value) should be (Json.obj(
      "name" -> Json.fromString("Bool")
    ))
  }
}
