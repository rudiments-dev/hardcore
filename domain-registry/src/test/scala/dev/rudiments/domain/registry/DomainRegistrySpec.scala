package dev.rudiments.domain.registry

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.another._
import io.circe.{Decoder, Encoder, Json}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable.ListMap


@RunWith(classOf[JUnitRunner])
class DomainRegistrySpec extends WordSpec with Matchers with ScalatestRouteTest with FailFastCirceSupport {
  private val skill = new DomainSkill()

  private implicit val en: Encoder[Instance] = new ThingEncoder(skill.domain).abstractInstanceEncoder("Thing")
  private implicit val de: Decoder[Instance] = new ThingDecoder(skill.domain).abstractInstanceDecoder("Thing")

  private val http = new HttpPort(
    "domain",
    ScalaTypes.ScalaString,
    i => ID(Seq(i.extract[String]("name"))),
    skill
  )(en, de)

  private val routes = Route.seal(http.routes)

  "get all Thing in Domain" in {
    Get("/domain") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Seq[Instance]].size should be (26)
    }
  }

  "get abstract Thing" in {
    Get("/domain/Thing") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (Instance(
        skill.domain.find[Spec]("Abstract"),
        Seq("Thing")
      ))
    }
  }

  "get The" in {
    Get("/domain/The") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (Instance(
        skill.domain.find[Spec]("Spec"),
        Seq(
          "The",
          "dev.rudiments.another.The",
          ListMap(
          "name" -> Instance(
            skill.valueSpec,
            Seq(
              Instance(
                skill.domain.find[Spec]("Text"),
                Seq(
                  Instance(
                    skill.domain.find[Spec]("Big"),
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
