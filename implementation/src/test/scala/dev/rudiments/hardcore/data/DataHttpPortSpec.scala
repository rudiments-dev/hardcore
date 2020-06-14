package dev.rudiments.hardcore.data

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import dev.rudiments.hardcore.data.ReadOnly._
import dev.rudiments.hardcore.http.CirceSupport._
import dev.rudiments.hardcore.types.{DTO, Defaults, HardID, HardType}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class DataHttpPortSpec extends WordSpec with Matchers with ScalatestRouteTest {
  private case class Example(
    id: Long = Defaults.long,
    name: String,
    enum: MyEnum
  ) extends DTO

  private implicit val actorSystem: ActorSystem = ActorSystem()
  private implicit val t: HardType[Example] = HardType[Example]
  private val repo: HardCache[Example] = new HardCache[Example]

  private val router: DataHttpPort[Example, Long] = new DataHttpPort(
    "example",
    e => HardID(e.id),
    repo
  )
  private val routes = Route.seal(router.routes)

  import MyEnum._
  private val sample = Example(42, "sample", Red)
  private val id = HardID(sample.id)

  "no element by ID" in {
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.NotFound)
    }
  }

  "put item into repository" in {
    Post("/example", sample) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
      responseAs[Example] should be (sample)
    }
  }

  "update item in repository" in {
    Put("/example/42", Example(42, "test", Red)) ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Example] should be (Example(42, "test", Red))
    }
  }

  "second create makes conflict" in {
    Post("/example", sample) ~> routes ~> check {
      response.status should be (StatusCodes.Conflict)
    }
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Example] should be (Example(42, "test", Red))
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
      Post("/example", Example(i, s"$i'th element", One)) ~> routes ~> check {
        response.status should be (StatusCodes.Created)
      }
    }
    repo(Count()).merge should be (Counted(10000))
    Get("/example/24") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Example] should be (Example(24, "24'th element", One))
    }
  }

  "endure 190.000 batch" in {
    Post("/example", (10001 to 200000).map(i => Example(i, s"$i'th element", Two))) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
      repo(Count()).merge should be (Counted(200000))
    }
    Get("/example/10024") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Example] should be (Example(10024, "10024'th element", Two))
    }
  }

  "clear repository" in {
    Delete("/example") ~> routes ~> check {
      response.status should be (StatusCodes.NoContent)
      repo(Count()).merge should be (Counted(0))
    }
  }

}
