package test.dev.rudiments.http

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import dev.rudiments.hardcore.ScalaTypes.ScalaLong
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.http.{CirceSupport, ScalaRouter}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder, Json}
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ScalaRouterSpec extends AnyWordSpec with Matchers with ScalatestRouteTest with CirceSupport {
  private val memory = new Memory()
  private implicit val actorSystem: ActorSystem = ActorSystem()

  private implicit val en: Encoder[Thing] = deriveEncoder[Smt].contramap { case d: Data => d.reconstruct[Smt]() }
  private implicit val dataEncoder: Encoder[Data] = deriveEncoder[Smt].contramap { case d: Data => d.reconstruct[Smt]() }
  private implicit val de: Decoder[Thing] = deriveDecoder[Smt].map(_.asData)

  private val router = new ScalaRouter[Smt](Path(ID("example")), ScalaLong, memory)

  private val routes = router.seal()
  private val sample: Data = Smt(42, "sample", None).asData

  "InstanceEncoder can encode" in {
    en(sample) should be (Json.obj(
      "id" -> Json.fromLong(42),
      "name" -> Json.fromString("sample"),
      "comment" -> Json.Null
    ))
  }

  "InstanceDecoder can decode" in {
    val decoded = de.decodeJson(Json.obj(
      "id" -> Json.fromLong(42),
      "name" -> Json.fromString("sample"),
      "comment" -> Json.Null
    )).getOrElse(throw new IllegalArgumentException("should exist"))
    decoded should be (sample)
  }

  "no element by ID" in {
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.NotFound)
    }
  }

  "put item into repository" in {
    Post("/example/42", sample) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
      responseAs[Thing] should be (sample)
    }
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Thing] should be (sample)
    }
  }

  "update item in repository" in {
    Put("/example/42", Smt(42L, "test", None).asData) ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Thing] should be (Smt(42L, "test", None).asData)
    }
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Thing] should be (Smt(42L, "test", None).asData)
    }
  }

  "second POST with same item conflicts with existing" in {
    Post("/example/42", Smt(42L, "test", None).asData) ~> routes ~> check {
      response.status should be (StatusCodes.Conflict)
    }
  }

  "delete items from repository" in {
    Delete("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.NoContent)
    }
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.NotFound)
    }
  }
}
