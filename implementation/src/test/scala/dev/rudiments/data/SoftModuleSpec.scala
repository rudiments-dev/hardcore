package dev.rudiments.data

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.domain._
import dev.rudiments.hardcore.All
import io.circe.{Decoder, Encoder, Json}
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SoftModuleSpec extends AnyWordSpec with Matchers with ScalatestRouteTest with FailFastCirceSupport {
  private case class Example(
    id: Long,
    name: String
  ) extends DTO
  
  private implicit val actorSystem: ActorSystem = ActorSystem()
  private implicit val domain: Domain = new Domain
  private implicit val t: Spec = domain.makeFromScala[Spec, Example]
  private val module = SoftModule("example", "id")
  private implicit val en: Encoder[Instance] = module.context.encoder
  private implicit val de: Decoder[Instance] = module.context.decoder

  private val routes = Route.seal(module.port.routes)
  private val sample: Instance = Instance(t, Seq(42L, "sample"))

  "Module encoder can encode" in {
    module.context.encoder.apply(sample) should be (Json.obj(
      "id" -> Json.fromLong(42),
      "name" -> Json.fromString("sample")
    ))
  }

  "Module decoder can decode" in {
    module.context.decoder.decodeJson(Json.obj(
      "id" -> Json.fromLong(42),
      "name" -> Json.fromString("sample")
    )).right.get should be (sample)
  }

  "no element by ID" in {
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.NotFound)
    }
  }

  "Create item into repository" in {
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
      response.status should be(StatusCodes.Conflict)
    }
  }

    "delete item from repository" in {
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
    module.context.adapter(Count(All)) should be (Counted(10000))
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (Instance(t, Seq(42L, "42'th element")))
    }
  }

  "endure 190.000 batch" in {
    Post("/example", (10001 to 200000).map(i => Instance(t, Seq(i.toLong, s"$i'th element")))) ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      module.context.adapter(Count(All)) should be (Counted(200000))
    }
    Get("/example/10042") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (Instance(t, Seq(10042L, "10042'th element")))
    }
  }

  "clear repository" in {
    Delete("/example") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      module.context.adapter(Count(All)) should be (Counted(0))
    }
  }

}
