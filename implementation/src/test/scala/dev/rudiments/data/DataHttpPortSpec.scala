package dev.rudiments.data

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.domain._
import dev.rudiments.hardcore.All
import dev.rudiments.hardcore.http.{ThingDecoder, ThingEncoder}
import io.circe.{Decoder, Encoder, Json}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class DataHttpPortSpec extends WordSpec with Matchers with ScalatestRouteTest with FailFastCirceSupport {
  private case class Example(
    id: Long,
    name: String
  ) extends DTO
  
  private implicit val actorSystem: ActorSystem = ActorSystem()
  private implicit val domain: Domain = new Domain
  private implicit val t: Spec = domain.makeFromScala[Spec, Example]
  private val state: State = new State

  private implicit val en: Encoder[Instance] = new ThingEncoder(domain).specEncoder(t)
  private implicit val de: Decoder[Instance] = new ThingDecoder(domain).specDecoder(t)

  private val router: DataHttpPort = new DataHttpPort(
    "example",
    ScalaTypes.ScalaLong,
    i => ID(Seq(i.extract[Long]("id"))),
    state
  )

  private val routes = Route.seal(router.routes)
  private val sample: Instance = Instance(t, Seq(42L, "sample"))

  "InstanceEncoder can encode" in {
    en.apply(sample) should be (Json.obj(
      "id" -> Json.fromLong(42),
      "name" -> Json.fromString("sample")
    ))
  }

  "InstanceDecoder can decode" in {
    de.decodeJson(Json.obj(
      "id" -> Json.fromLong(42),
      "name" -> Json.fromString("sample")
    )).right.get should be (sample)
  }

  "no element by ID" in {
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.NotFound)
    }
  }

  "put item into repository" in {
    Post("/example", sample) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
      responseAs[Instance] should be (sample)
    }
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (sample)
    }
  }

  "update item in repository" in {
    Put("/example/42", Instance(t, Seq(42L, "test"))) ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (Instance(t, Seq(42L, "test")))
    }
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (Instance(t, Seq(42L, "test")))
    }
  }

  "move item in repository" in {
    Put("/example/42", Instance(t, Seq(24L, "test"))) ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (Instance(t, Seq(24L, "test")))
    }
    Get("/example/24") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (Instance(t, Seq(24L, "test")))
    }
  }

  "second POST with same item conflicts with existing" in {
    Post("/example", Instance(t, Seq(24L, "test"))) ~> routes ~> check {
      response.status should be (StatusCodes.Conflict)
    }
  }

  "delete items from repository" in {
    Delete("/example/24") ~> routes ~> check {
      response.status should be (StatusCodes.NoContent)
    }
    Get("/example/24") ~> routes ~> check {
      response.status should be (StatusCodes.NotFound)
    }
  }

  "endure 10.000 records" in {
    (1 to 10000).foreach { i =>
      Post("/example", Instance(t, Seq(i.toLong, s"$i'th element"))) ~> routes ~> check {
        response.status should be (StatusCodes.Created)
      }
    }
    state(Count(All)) should be (Counted(10000))
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (Instance(t, Seq(42L, "42'th element")))
    }
  }

  "endure 190.000 batch" in {
    Post("/example", (10001 to 200000).map(i => Instance(t, Seq(i.toLong, s"$i'th element")))) ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      state(Count(All)) should be (Counted(200000))
    }
    Get("/example/10042") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (Instance(t, Seq(10042L, "10042'th element")))
    }
  }

  "clear repository" in {
    Delete("/example") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      state(Count(All)) should be (Counted(0))
    }
  }

}
