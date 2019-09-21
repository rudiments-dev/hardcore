package dev.rudiments.hardcore.data

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import dev.rudiments.hardcore.http.CirceSupport._
import dev.rudiments.hardcore.http.IDPath
import dev.rudiments.hardcore.types.{DTO, ID, Type}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class DataHttpPortSpec extends WordSpec with Matchers with ScalatestRouteTest {
  private case class Example(
    id: Long,
    name: String
  ) extends DTO

  private implicit val actorSystem: ActorSystem = ActorSystem()
  private implicit val t: Type[Example] = Type[Example]
  private val repo: DataMemoryAdapter[Example] = new DataMemoryAdapter[Example]

  private val router: DataHttpPort[Example] = new DataHttpPort[Example](
    "example",
    IDPath[Example, Long],
    e => ID[Example, Long](e.id),
    repo
  )
  private val routes = Route.seal(router.routes)
  private val sample = Example(42, "sample")
  private val id = ID(sample.id)

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
    Put("/example/42", Example(42, "test")) ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Example] should be (Example(42, "test"))
    }
  }

  "second create makes conflict" in {
    Post("/example", sample) ~> routes ~> check {
      response.status should be (StatusCodes.Conflict)
    }
  }

  "delete item from repository" in {
    Delete("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.NoContent)
    }
  }

  "endure 10.000 records" in {
    (1 to 10000).foreach { i =>
      Post("/example", Example(i, s"$i'th element")) ~> routes ~> check {
        response.status should be (StatusCodes.Created)
      }
    }
    repo(Count()(t)) should be (Counted(10000))
  }

  "endure 190.000 batch" in {
    Put("/example", (10001 to 200000).map(i => Example(i, s"$i'th element"))) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
      repo(Count()(t)) should be (Counted(200000))
    }
  }

  "clear repository" in {
    Delete("/example") ~> routes ~> check {
      response.status should be (StatusCodes.NoContent)
      repo(Count()(t)) should be (Counted(0))
    }
  }

}
