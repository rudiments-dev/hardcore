package dev.rudiments.hardcore.http

import dev.rudiments.domain._
import io.circe.Json
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ThingDecoderTest extends AnyWordSpec with Matchers {

  case class Test(f : Option[Double]) extends DTO

  private val domain = new Domain
  domain.makeFromScala[Thing, Thing]
  domain.makeFromScala[Spec, Test]
  domain.makeFromScala[Spec, AlgebraicExample]
  val decoder = new ThingDecoder(domain)

  "decoder class with option flag" in {
    decoder.specDecoder("Test").decodeJson(Json.obj("f" -> Json.Null)) should be (Right(
      domain.find[Spec]("Test").fromProduct(domain, Test(None))
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
      domain
        .find[Spec]("AlgebraicExample")
        .fromProduct(domain, AlgebraicExample(B))
    ))

    decoder.specDecoder("AlgebraicExample")
      .decodeJson(Json.obj(
        "a" -> Json.obj(
          "type" -> Json.fromString("C"),
          "s" -> Json.fromString("something algebraic")
        )
      )) should be (Right(
      domain
        .find[Spec]("AlgebraicExample")
        .fromProduct(domain, AlgebraicExample(C("something algebraic")))
    ))
  }

  "decode Spec" in {
    val spec = domain.find[Spec]("Spec")

    val textJson: Json = Json.obj(
      "type" -> Json.fromString("Text"),
      "maxSize" -> Json.obj(
        "type" -> Json.fromString("Big"),
        "size" -> Json.fromInt(Int.MaxValue)
      )
    )

    decoder.specDecoder("Spec").decodeJson(Json.obj(
      "name" -> Json.fromString("Spec"),
      "fullName" -> Json.fromString("dev.rudiments.domain.Spec"),
      "fields" -> Json.obj(
        "name" -> Json.obj(
          "thing" -> textJson,
          "isRequired" -> Json.True
        ),
        "fullName" -> Json.obj(
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
              "fullName" -> Json.fromString("dev.rudiments.domain.ValueSpec"),
              "fields" -> Json.obj(
                "thing" -> Json.obj(
                  "thing" -> Json.obj(
                    "type" -> Json.fromString("Abstract"),
                    "name" -> Json.fromString("Thing"),
                    "fields" -> Json.obj(
                      "name" -> Json.obj(
                        "thing" -> textJson,
                        "isRequired" -> Json.True
                      )
                    )
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
    )) should be (Right(spec.fromProduct(domain, spec)))
  }

  "decode Abstract Thing" in {
    val spec = domain.find[Spec]("Abstract")
    val t = domain.find[Abstract]("Thing")

    decoder.specDecoder("Abstract").decodeJson(Json.obj(
      "name" -> Json.fromString("Thing"),
      "fields" -> Json.obj(
        "name" -> Json.obj(
          "thing" -> Json.obj(
            "type" -> Json.fromString("Text"),
            "maxSize" -> Json.obj(
              "type" -> Json.fromString("Big"),
              "size" -> Json.fromInt(Int.MaxValue)
            )
          ),
          "isRequired" -> Json.True
        )
      )
    )) should be (Right(spec.fromProduct(domain, t)))
  }

  "decode Plain" in {
    val spec = domain.find[Spec]("The")
    val t = domain.find[The]("Bool")

    decoder.specDecoder("The").decodeJson(Json.obj(
      "name" -> Json.fromString("Bool")
    )) should be (Right(spec.fromProduct(domain, t)))
  }
}
