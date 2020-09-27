package dev.rudiments.domain.registry

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.data.DataHttpPort
import dev.rudiments.domain._
import dev.rudiments.hardcore.http.{ThingDecoder, ThingEncoder}
import io.circe.{Decoder, Encoder, Json}
import org.junit.Ignore
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable.ListMap


@RunWith(classOf[JUnitRunner])
class DomainRegistrySpec extends WordSpec with Matchers with ScalatestRouteTest with FailFastCirceSupport {
  private val domain = new DomainModule

  private implicit val en: Encoder[Instance] = domain.encoder
  private implicit val de: Decoder[Instance] = domain.decoder

  private val routes = Route.seal(domain.http.routes)

  "get all Thing in Domain" in {
    Get("/domain") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Seq[Instance]].size should be (27)
    }
  }

  "get abstract Thing" in {
    Get("/domain/Thing") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (
        Instance(domain("Type"),
          Seq(
            "Thing",
            Abstract("Thing", ListMap("name" -> ValueSpec(ScalaTypes.ScalaString, true))),
            Seq.empty
          )
        )
      )
    }
  }

  "get The" in {
    Get("/domain/The") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (
        Instance(domain("Type"),
          Seq(
            "The",
            Instance(domain("Spec"),
              Seq(
                "The",
                "dev.rudiments.domain.The",
                ListMap(
                  "name" -> Instance(
                    domain.skill.valueSpec,
                    Seq(
                      Instance(
                        domain("Text"),
                        Seq(
                          Instance(
                            domain("Big"),
                            Seq(Int.MaxValue)
                          )
                        )
                      ),
                      true
                    )
                  )
                ))
            ),
            Seq("Thing")
          )
        )
      )
    }
  }

  "get Spec" in {
    Get("/domain/Spec") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (
        Instance(domain("Type"),
          Seq(
            "Spec",
            Instance(domain("Spec"),
              Seq(
                "Spec",
                "dev.rudiments.domain.Spec",
                ListMap(
                  "name" -> Instance(domain.skill.valueSpec,
                    Seq(Instance(domain("Text"), Seq(Instance(domain("Big"), Seq(Int.MaxValue)))), true)
                  ),
                  "fullName" -> Instance(domain.skill.valueSpec,
                    Seq(Instance(domain("Text"), Seq(Instance(domain("Big"), Seq(Int.MaxValue)))), true)
                  ),
                  "fields" -> Instance(domain.skill.valueSpec,
                    Seq(
                      Instance(domain("Index"),
                        Seq(
                          Instance(domain("Text"), Seq(Instance(domain("Big"), Seq(Int.MaxValue)))),
                          Instance(domain("Spec"),
                            Seq(
                              "ValueSpec",
                              "dev.rudiments.domain.ValueSpec",
                              ListMap(
                                "thing" -> Instance(domain("ValueSpec"),
                                  Seq(Abstract("Thing", ListMap("name" -> ValueSpec(ScalaTypes.ScalaString, true))), true)
                                ),
                                "isRequired" -> Instance(domain("ValueSpec"),
                                  Seq(
                                    The("Bool"),
                                    true
                                  )
                                )
                              )
                            ))
                        )
                      ),
                      true
                    )
                  )
                ))
            ),
            Seq("Thing")
          )
        )
      )
    }
  }

  "create SampleSpec" in {
    val content = Json.obj(
      "name" -> Json.fromString("SpecExample"),
      "thing" -> Json.obj(
        "type" -> Json.fromString("Spec"),
        "name" -> Json.fromString("SpecExample"),
        "fullName" -> Json.fromString("dev.rudiments.domain.registry.DomainRegistrySpec.SpecExample"),
        "fields" -> Json.obj(
          "b" -> Json.obj(
            "thing" -> Json.obj("type" -> Json.fromString("Bool")),
            "isRequired" -> Json.True
          )
        )
      ),
      "is" -> Json.arr()
    )

    Post("/domain", content) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
    }
  }
}
