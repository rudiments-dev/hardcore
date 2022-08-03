package test.dev.rudiments.http

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.http.ThingEncoder.discriminator
import dev.rudiments.hardcore.http.{CirceSupport, ScalaRouter}
import io.circe.Json
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ScalaRouterSpec extends AnyWordSpec with Matchers with ScalatestRouteTest with CirceSupport {
  private implicit val actorSystem: ActorSystem = ActorSystem()
  private val ctx: Context = new Context()

  private val router = new ScalaRouter(ctx)
  private val routes = router.seal("example")
  private val t = Type(
    Field("id", Number(Long.MinValue, Long.MaxValue)),
    Field("name", Text(Int.MaxValue)),
    Field("comment", Text(Int.MaxValue))
  )
  private val sample: Data = t.data(42, "sample", None)

  private val types = ID("types")

  "can encode links" in {
    router.thingEncoder(ctx ! (types / "Bool")) should be (Json.fromString("Bool"))
    router.thingEncoder(ctx ! (types / "Number")) should be (Json.obj(
      "type" -> Json.fromString("Number")
    ))
  }

  "can encode predicates" in {
    router.thingEncoder(ctx ? (types / "Bool")) should be(Json.obj(
      "type" -> Json.fromString("Nothing")
    ))
    router.thingEncoder(ctx ? (types / "Number")) should be(Json.obj(
      "type" -> Json.fromString("Type"),
      "from" -> Json.obj("type" -> Json.fromString("Anything")),
      "to" -> Json.obj("type" -> Json.fromString("Anything"))
    ))
  }

  "dataEncoder can encode" in {
    router.thingEncoder(sample) should be (Json.obj(
      "id" -> Json.fromInt(42),
      "name" -> Json.fromString("sample"),
      "comment" -> Json.Null
    ))
  }

  "no element by ID" in {
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.NotFound)
    }
  }

  "put item into repository" in {
    val c = Commit(
      Map(ID("42") -> Created(sample))
    )
    ctx << c should be (Committed(c))
//    Post("/example/42", sample) ~> routes ~> check {
//      response.status should be (StatusCodes.Created)
//      responseAs[Data] should be (sample)
//    }
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
//      responseAs[Data] should be (sample)
    }
  }
//
//  "update item in repository" in {
//    Put("/example/42", Smt(42L, "test", None).asData) ~> routes ~> check {
//      response.status should be (StatusCodes.OK)
//      responseAs[Thing] should be (Smt(42L, "test", None).asData)
//    }
//    Get("/example/42") ~> routes ~> check {
//      response.status should be (StatusCodes.OK)
//      responseAs[Thing] should be (Smt(42L, "test", None).asData)
//    }
//  }
//
//  "second POST with same item conflicts with existing" in {
//    Post("/example/42", Smt(42L, "test", None).asData) ~> routes ~> check {
//      response.status should be (StatusCodes.Conflict)
//    }
//  }

  "delete items from repository" in {
    Delete("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.NoContent)
    }
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.NotFound)
    }
  }
}
