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
  private val cache: SoftCache = new SoftCache

  private val router: DataHttpPort = new DataHttpPort(
    "example",
    "id",
    i => SoftID(t.extract(i, "id")),
    cache
  )(
    t,
    SoftEncoder(t).contramap { case i: SoftInstance => i },
    SoftDecoder(t).map(_.asInstanceOf[Instance])
  )
  private implicit val en: Encoder[SoftInstance] = SoftEncoder(t)
  private implicit val de: Decoder[SoftInstance] = SoftDecoder(t)

  private val routes = Route.seal(router.routes)
  private val sample = SoftInstance(42L, "sample")

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
    Get("/example/1") ~> routes ~> check {
      response.status should be (StatusCodes.NotFound)
    }
  }

  "put item into repository" in {
    Post("/example", sample) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
      responseAs[SoftInstance] should be (sample)
    }
    Get("/example/1") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[SoftInstance] should be (sample)
    }
  }

  "update item in repository" in {
    Put("/example/1", SoftInstance(42L, "test")) ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[SoftInstance] should be (SoftInstance(42L, "test"))
    }
    Get("/example/1") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[SoftInstance] should be (SoftInstance(42L, "test"))
    }
  }

  "second POST creates another item" in {
    Post("/example", sample) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
    }
    Get("/example/2") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[SoftInstance] should be (sample)
    }
  }

  "delete items from repository" in {
    Delete("/example/1") ~> routes ~> check {
      response.status should be (StatusCodes.NoContent)
    }
    Get("/example/1") ~> routes ~> check {
      response.status should be (StatusCodes.NotFound)
    }
    Delete("/example/2") ~> routes ~> check {
      response.status should be (StatusCodes.NoContent)
    }
    Get("/example/2") ~> routes ~> check {
      response.status should be (StatusCodes.NotFound)
    }
  }

  "endure 10.000 records" in {
    (1 to 10000).foreach { i =>
      Post("/example", SoftInstance(i.toLong, s"$i'th element")) ~> routes ~> check {
        response.status should be (StatusCodes.Created)
      }
    }
    cache(Count).merge should be (Counted(10000))
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[SoftInstance] should be (SoftInstance(40L, "40'th element"))
    }
  }

  "endure 190.000 batch" in {
    Post("/example", (10003 to 200002).map(i => SoftInstance(i.toLong, s"$i'th element"))) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
      cache(Count).merge should be (Counted(200000))
    }
    Get("/example/10042") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[SoftInstance] should be (SoftInstance(10042L, "10042'th element"))
    }
  }

  "clear repository" in {
    Delete("/example") ~> routes ~> check {
      response.status should be (StatusCodes.NoContent)
      cache(Count).merge should be (Counted(0))
    }
  }

}
