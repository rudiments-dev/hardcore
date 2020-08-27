package dev.rudiments.another

import io.circe.Json
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class ThingDecoderTest extends WordSpec with Matchers {

  case class Test(f : Option[Double]) extends DTO

  private val typeSystem = TypeSystem()
  typeSystem.makeFromScala[Thing, Thing]
  typeSystem.makeFromScala[Spec, Test]
  typeSystem.makeFromScala[Spec, AlgebraicExample]
  val decoder = new ThingDecoder(typeSystem)

  "decoder class with option flag" in {
    decoder.specDecoder("Test").decodeJson(Json.obj("f" -> Json.Null)) should be (Right(
      typeSystem.find[Spec]("Test").fromProduct(typeSystem, Test(None))
    ))
  }

  "decode ADT" in {
    decoder
      .specDecoder("AlgebraicExample")
      .decodeJson(Json.obj(
        "a" -> Json.obj(
          "type" -> Json.fromString("B")
        )
      )) should be (Right(
      typeSystem
        .find[Spec]("AlgebraicExample")
        .fromProduct(typeSystem, AlgebraicExample(B))
    ))

    decoder.specDecoder("AlgebraicExample")
      .decodeJson(Json.obj(
        "a" -> Json.obj(
          "type" -> Json.fromString("C"),
          "s" -> Json.fromString("something algebraic")
        )
      )) should be (Right(
      typeSystem
        .find[Spec]("AlgebraicExample")
        .fromProduct(typeSystem, AlgebraicExample(C("something algebraic")))
    ))
  }

  "decode Spec" in {
    val spec = typeSystem.find[Spec]("Spec")

    val textJson: Json = Json.obj(
      "type" -> Json.fromString("Text"),
      "maxSize" -> Json.obj(
        "type" -> Json.fromString("Big"),
        "size" -> Json.fromInt(Int.MaxValue)
      )
    )

    decoder.specDecoder("Spec").decodeJson(Json.obj(
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
    )) should be (Right(spec.fromProduct(typeSystem, spec)))
  }

  "decode Abstract Thing" in {
    val spec = typeSystem.find[Spec]("Abstract")
    val t = typeSystem.find[Abstract]("Thing")

    decoder.specDecoder("Abstract").decodeJson(Json.obj(
      "name" -> Json.fromString("Thing")
    )) should be (Right(spec.fromProduct(typeSystem, t)))
  }

  "decode Plain" in {
    val spec = typeSystem.find[Spec]("The")
    val t = typeSystem.find[The]("Bool")

    decoder.specDecoder("The").decodeJson(Json.obj(
      "name" -> Json.fromString("Bool")
    )) should be (Right(spec.fromProduct(typeSystem, t)))
  }
}
