package dev.rudiments.data

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.data.ReadOnly._
import dev.rudiments.hardcore.types._
import io.circe.{Decoder, Encoder, Json}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class SoftModuleSpec extends WordSpec with Matchers with ScalatestRouteTest with FailFastCirceSupport {
  private case class Example(
    id: Long = Defaults.long,
    name: String
  ) extends DTO
  
  private implicit val actorSystem: ActorSystem = ActorSystem()
  private implicit val t: Type = ScalaType[Example]
  private val module = SoftModule("example", "id")
  private implicit val en: Encoder[Instance] = module.context.encoder
  private implicit val de: Decoder[Instance] = module.context.decoder

  private val routes = Route.seal(module.port.routes)
  private val sample: Instance = SoftInstance(42L, "sample")

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
    Put("/example/42", SoftInstance(42L, "test").asInstanceOf[Instance]) ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (SoftInstance(42L, "test"))
    }
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (SoftInstance(42L, "test"))
    }
  }

  "second POST with same item conflicts with existing" in {
    Post("/example", sample) ~> routes ~> check {
      response.status should be(StatusCodes.Conflict)
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
      Post("/example", SoftInstance(i.toLong, s"$i'th element").asInstanceOf[Instance]) ~> routes ~> check {
        response.status should be (StatusCodes.Created)
      }
    }
    module.context.adapter(Count).merge should be (Counted(10000))
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (SoftInstance(42L, "42'th element"))
    }
  }

  "endure 190.000 batch" in {
    Post("/example", (10001 to 200000).map(i => SoftInstance(i.toLong, s"$i'th element").asInstanceOf[Instance])) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
      module.context.adapter(Count).merge should be (Counted(200000))
    }
    Get("/example/10042") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (SoftInstance(10042L, "10042'th element"))
    }
  }

  "clear repository" in {
    Delete("/example") ~> routes ~> check {
      response.status should be (StatusCodes.NoContent)
      module.context.adapter(Count).merge should be (Counted(0))
    }
  }

}
