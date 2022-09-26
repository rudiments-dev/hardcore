package test.dev.rudiments.http

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import dev.rudiments.hardcore.Initial.types
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.http.{CirceSupport, ScalaRouter, ThingDecoder}
import io.circe.Decoder
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

  private val mem = new Memory()
  private val ts = new TypeSystem(mem /! types)
  private val td = new ThingDecoder(ts)
  private val router = new ScalaRouter(mem.node)(td)
  private val routes = router.seal()
  private implicit val de: Decoder[Data] = td.dataTypeDecoder(t)

  mem += ID("example") -> Node(Nothing, leafIs = t)
  mem += ID("34") -> Node(Nothing, leafIs = Nothing)
  mem += (ID("34") / "43") -> Node(Nothing, leafIs = t)

  private val sample = t.data(42, "sample", "non-optional comment")

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
    mem ? (ID("example") / "42") should be (Readen(sample))

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

  "can create deep into memory" in {
    val sample2 = t.data(0L, "deep", "test")

    val path = ID("34") / "43" / "10"

    Get("/34/43") ~> routes ~> check {
      response.status should be(StatusCodes.OK)
    }

    Post("/34/43/10", sample2) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
      responseAs[Data] should be (sample2)
      mem ? path should be (Readen(sample2))
    }
  }
}
