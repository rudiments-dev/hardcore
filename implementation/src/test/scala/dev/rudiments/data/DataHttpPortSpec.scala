package dev.rudiments.data

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.data.ReadOnly._
import dev.rudiments.hardcore.http.{SoftDecoder, SoftEncoder}
import dev.rudiments.hardcore.types._
import io.circe.{Decoder, Encoder, Json}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class DataHttpPortSpec extends WordSpec with Matchers with ScalatestRouteTest with FailFastCirceSupport {
  private case class Example(
    id: Long = Defaults.long,
    name: String
  ) extends DTO
  
  private implicit val actorSystem: ActorSystem = ActorSystem()
  private implicit val t: Type = HardType[Example]
  private val repo: MemoryAdapter = new MemoryAdapter

  private val router: DataHttpPort = new DataHttpPort(
    "example",
    "id",
    i => SoftID(t.extract(i, "id")),
    repo
  )
  private implicit val en: Encoder[SoftInstance] = SoftEncoder(t)
  private implicit val de: Decoder[SoftInstance] = SoftDecoder(t)

  private val routes = Route.seal(router.routes)
  private val sample = t.softFromHard(Example(42, "sample"))
  private val id = SoftID(t.extract(sample, "id"))

  "SoftEncoder can encode" in {
    en.apply(sample) should be (Json.obj(
      "id" -> Json.fromLong(42),
      "name" -> Json.fromString("sample")
    ))
  }

  "SoftDecoder can decode" in {
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
      responseAs[SoftInstance] should be (sample)
    }
  }

  "update item in repository" in {
    Put("/example/42", SoftInstance(42L, "test")) ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[SoftInstance] should be (SoftInstance(42, "test"))
    }
  }

  "second create makes conflict" in {
    Post("/example", sample) ~> routes ~> check {
      response.status should be (StatusCodes.Conflict)
    }
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[SoftInstance] should be (SoftInstance(42, "test"))
    }
  }

  "delete item from repository" in {
    Delete("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.NoContent)
    }
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.NotFound)
    }
  }

  "endure 10.000 records" in {
    (1 to 10000).foreach { i =>
      Post("/example", SoftInstance(i.toLong, s"$i'th element")) ~> routes ~> check {
        response.status should be (StatusCodes.Created)
      }
    }
    repo(Count) should be (Counted(10000))
    Get("/example/24") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[SoftInstance] should be (SoftInstance(24L, "24'th element"))
    }
  }

  "endure 190.000 batch" in {
    Post("/example", (10001 to 200000).map(i => SoftInstance(i.toLong, s"$i'th element"))) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
      repo(Count) should be (Counted(200000))
    }
    Get("/example/10024") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[SoftInstance] should be (SoftInstance(10024L, "10024'th element"))
    }
  }

  "clear repository" in {
    Delete("/example") ~> routes ~> check {
      response.status should be (StatusCodes.NoContent)
      repo(Count) should be (Counted(0))
    }
  }

}
