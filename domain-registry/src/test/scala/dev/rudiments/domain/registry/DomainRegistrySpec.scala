package dev.rudiments.domain.registry

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.data.DataHttpPort
import dev.rudiments.domain._
import dev.rudiments.hardcore.http.{ThingDecoder, ThingEncoder}
import io.circe.{Decoder, Encoder, Json}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable.ListMap


@RunWith(classOf[JUnitRunner])
class DomainRegistrySpec extends WordSpec with Matchers with ScalatestRouteTest with FailFastCirceSupport {
  private val domain = new DomainModule

  private implicit val en: Encoder[Instance] = new ThingEncoder(domain.ctx.domain).abstractInstanceEncoder("Thing")
  private implicit val de: Decoder[Instance] = new ThingDecoder(domain.ctx.domain).abstractInstanceDecoder("Thing")

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
      responseAs[Instance] should be (Instance(domain("Abstract"), Seq(
        "Thing", ListMap(
          "name" -> Instance(domain("ValueSpec"), Seq(
            Instance(domain("Text"), Seq(
              Instance(domain("Big"), Seq(BigDecimal(Int.MaxValue)))
            )),
            true
          ))
        )
      )))
    }
  }

  "get The" in {
    Get("/domain/The") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (Instance(
        domain("Spec"),
        Seq(
          "The",
          "dev.rudiments.domain.The",
          ListMap(
          "name" -> Instance(
            domain.ctx.valueSpec,
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
      ))
    }
  }

  "get Spec" in {
    Get("/domain/Spec") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
//      responseAs[Json] should be (Too big to make any sense)
    }
  }

  "create SampleSpec" in {
    val content = Json.obj(
      "type" -> Json.fromString("Spec"),
      "name" -> Json.fromString("SpecExample"),
      "fullName" -> Json.fromString("dev.rudiments.domain.registry.DomainRegistrySpec.SpecExample"),
      "fields" -> Json.obj(
        "b" -> Json.obj(
          "thing" -> Json.obj("type" -> Json.fromString("Bool")),
          "isRequired" -> Json.True
        )
      )
    )
    Post("/domain", content) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
    }
  }
}
