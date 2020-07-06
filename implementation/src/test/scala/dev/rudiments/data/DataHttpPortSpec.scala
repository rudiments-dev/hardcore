package dev.rudiments.data

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.data.ReadOnly._
import dev.rudiments.hardcore.http.{InstanceDecoder, InstanceEncoder}
import dev.rudiments.types._
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
  private implicit val typeSystem: TypeSystem = TypeSystem()
  private implicit val t: Type = typeSystem.asType[Example]
  private val cache: SoftCache = new SoftCache

  private implicit val en: Encoder[Instance] = new InstanceEncoder(typeSystem)(t)
  private implicit val de: Decoder[Instance] = new InstanceDecoder(typeSystem)(t)

  private val router: DataHttpPort = new DataHttpPort(
    "example",
    "id",
    i => i.extractID[Any]("id"),
    cache
  )

  private val routes = Route.seal(router.routes)
  private val sample: Instance = Instance(42L, "sample")

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
    Put("/example/42", Instance(42L, "test")) ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (Instance(42L, "test"))
    }
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (Instance(42L, "test"))
    }
  }

  "second POST with same item conflicts with existing" in {
    Post("/example", sample) ~> routes ~> check {
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

  "endure 10.000 records" in {
    (1 to 10000).foreach { i =>
      Post("/example", Instance(i.toLong, s"$i'th element")) ~> routes ~> check {
        response.status should be (StatusCodes.Created)
      }
    }
    cache(Count()).merge should be (Counted(10000))
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (Instance(42L, "42'th element"))
    }
  }

  "endure 190.000 batch" in {
    Post("/example", (10001 to 200000).map(i => Instance(i.toLong, s"$i'th element"))) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
      cache(Count()).merge should be (Counted(200000))
    }
    Get("/example/10042") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (Instance(10042L, "10042'th element"))
    }
  }

  "clear repository" in {
    Delete("/example") ~> routes ~> check {
      response.status should be (StatusCodes.NoContent)
      cache(Count()).merge should be (Counted(0))
    }
  }

}
