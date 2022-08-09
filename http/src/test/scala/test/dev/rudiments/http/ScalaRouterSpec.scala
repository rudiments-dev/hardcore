package test.dev.rudiments.http

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.http.{CirceSupport, ScalaRouter}
import io.circe.{Decoder, Json}
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ScalaRouterSpec extends AnyWordSpec with Matchers with ScalatestRouteTest with CirceSupport {
  private implicit val actorSystem: ActorSystem = ActorSystem()
  private val t = Type(
    Field("id", Number(Long.MinValue, Long.MaxValue)),
    Field("name", Text(Int.MaxValue)),
    Field("comment", Text(Int.MaxValue))
  )

  private val mem: Memory = new Memory(Nothing, leafIs = t)
  private val router = new ScalaRouter(mem)
  private val routes = router.seal("example")
  private implicit val de: Decoder[Data] = router.de

  private val sample: Thing = t.data(42, "sample", "non-optional comment")

  "no element by ID" in {
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.NotFound)
      mem ? ID("42") should be (NotExist)
    }
  }

  "put item into repository" in {
    Post("/example/42", sample) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
      responseAs[Data] should be (sample)
    }
    mem ? ID("42") should be (Readen(sample))

    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Data] should be (sample)
    }
  }

  "update item in repository" in {
    Put("/example/42", t.data(42L, "test", "non-optional comment")) ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Data] should be (t.data(42L, "test", "non-optional comment"))
    }
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Data] should be (t.data(42L, "test", "non-optional comment"))
    }
  }

  "second POST with same item conflicts with existing" in {
    Post("/example/42", t.data(42L, "test", "non-optional comment")) ~> routes ~> check {
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
