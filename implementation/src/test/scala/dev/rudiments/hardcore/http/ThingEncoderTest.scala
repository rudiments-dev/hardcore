package dev.rudiments.hardcore.http

import dev.rudiments.domain._
import io.circe.Json
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class ThingEncoderTest extends WordSpec with Matchers {

  case class Test(f : Option[Double]) extends DTO

  private val domain = Domain()
  domain.makeFromScala[Thing, Thing]
  domain.makeFromScala[Spec, Test]
  domain.makeFromScala[Spec, AlgebraicExample]
  val encoder = new ThingEncoder(domain)

  "encode class with option flag" in {
    val value = domain.find[Spec]("Test").fromProduct(domain, Test(None))
    encoder.specEncoder("Test")(value) should be (Json.obj("f" -> Json.Null))
  }

  "encode ADT" in {
    val valueB = domain
      .find[Spec]("AlgebraicExample")
      .fromProduct(domain, AlgebraicExample(B))
    encoder.specEncoder("AlgebraicExample")(valueB) should be (Json.obj(
      "a" -> Json.obj("type" -> Json.fromString("B"))
    ))

    val valueC = domain
      .find[Spec]("AlgebraicExample")
      .fromProduct(domain, AlgebraicExample(C("something algebraic")))
    encoder.specEncoder("AlgebraicExample")(valueC) should be (Json.obj(
      "a" -> Json.obj(
        "type" -> Json.fromString("C"),
        "s" -> Json.fromString("something algebraic")
      )
    ))
  }

  "encode Spec" in {
    val spec = domain.find[Spec]("Spec")
    val value = spec.fromProduct(domain, spec)

    val textJson: Json = Json.obj(
      "type" -> Json.fromString("Text"),
      "maxSize" -> Json.obj(
        "type" -> Json.fromString("Big"),
        "size" -> Json.fromInt(Int.MaxValue)
      )
    )

    encoder.specEncoder("Spec")(value) should be (Json.obj(
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
    ))
  }

  "encode Abstract Thing" in {
    val spec = domain.find[Spec]("Abstract")
    val t = domain.find[Abstract]("Thing")
    val value = spec.fromProduct(domain, t)

    encoder.specEncoder("Abstract")(value) should be (Json.obj(
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
    ))
  }

  "encode Plain" in {
    val spec = domain.find[Spec]("The")
    val t = domain.find[The]("Bool")
    val value = spec.fromProduct(domain, t)

    encoder.specEncoder("The")(value) should be (Json.obj(
      "name" -> Json.fromString("Bool")
    ))
  }
}
