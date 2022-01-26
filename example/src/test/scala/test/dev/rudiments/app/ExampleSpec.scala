package test.dev.rudiments.app

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import dev.rudiments.app.{Body, Example}
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.http.CirceSupport
import io.circe.{Decoder, Json}
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExampleSpec extends AnyWordSpec with Matchers with ScalatestRouteTest with CirceSupport {
  private implicit val actorSystem: ActorSystem = ActorSystem()
  private implicit val space: Space = new Space()
  private val example = new Example()
  private val router = example.router
  private val routes = Route.seal(router.routes)
  private implicit val de: Decoder[Thing] = example.decoder

  private val sample: Data = Body("some name", Seq("some data")).asData

  "InstanceEncoder can encode" in {
    thingEncoder(sample) should be (Json.obj(
      "name" -> Json.fromString("some name"),
      "strings" -> Json.arr(Json.fromString("some data"))
    ))
  }

  "InstanceDecoder can decode" in {
    val decoded = de.decodeJson(Json.obj(
      "name" -> Json.fromString("some name"),
      "strings" -> Json.arr(Json.fromString("some data"))
    )).getOrElse(throw new IllegalArgumentException("should exist"))
    decoded should be (sample)
  }

  "no element by ID" in {
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.NotFound)
    }
  }

  "put item into repository" in {
    Post("/example/42", sample.asInstanceOf[Thing]) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
      responseAs[Thing] should be (sample)
    }
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Thing] should be (sample)
    }
  }

  "update item in repository" in {
    Put("/example/42", Body("name", Seq("some", "data")).asData.asInstanceOf[Thing]) ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Thing] should be (Body("name", Seq("some", "data")).asData)
    }
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Thing] should be (Body("name", Seq("some", "data")).asData)
    }
  }

  "second POST with same item conflicts with existing" in {
    Post("/example/42", Body("test", Seq.empty).asData.asInstanceOf[Thing]) ~> routes ~> check {
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
