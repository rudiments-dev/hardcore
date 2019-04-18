package dev.rudiments.hardcore.repo.actor.spec

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import dev.rudiments.hardcore.dsl.ID._
import dev.rudiments.hardcore.dsl._
import dev.rudiments.hardcore.eventstore.ActorEventStore
import dev.rudiments.hardcore.http.CirceSupport._
import dev.rudiments.hardcore.http.{CrudRouter, IDPath}
import dev.rudiments.hardcore.repo.actor.ActorDataHandler
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{AsyncFlatSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class CrudRouterViaActorSpec extends AsyncFlatSpec with Matchers with ScalatestRouteTest {
  case class Example(
    id: Long,
    name: String
  )

  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val meta: Meta[Example] = Meta(value => ID(value.id))
  implicit val es: ActorEventStore = new ActorEventStore

  val handler: ActorDataHandler[Example] = new ActorDataHandler[Example]

  val router: CrudRouter[Example] = new CrudRouter[Example]("example", handler, IDPath[Example, Long])
  val routes = Route.seal(router.routes)
  val sample = Example(42, "sample")
  val id = sample.identify

  it should "no element by ID" in {
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.NotFound)
      es.state().map(s => s.size should be (1))
    }
  }

  it should "put item into repository" in {
    Post("/example", sample) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
      responseAs[Example] should be (sample)
      es.state().map(s => s.size should be (2))
    }
  }

  it should "update item in repository" in {
    Put("/example/42", Example(42, "test")) ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Example] should be (Example(42, "test"))
      es.state().map(s => s.size should be (3))
    }
  }

  it should "multiple commands with same ID returns same result" in {
    Post("/example", sample) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
      es.state().map(s => s.size should be (3))
    }
  }

  it should "delete item from repository" in {
    Delete("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.NoContent)
      es.state().map(s => s.size should be (4))
    }
  }

  it should "endure 10.000 records" in {
    (1 to 10000).foreach { i =>
      Post("/example", Example(i, s"$i'th element")) ~> routes ~> check {
        response.status should be (StatusCodes.Created)
      }
    }
    es.state().map(s => s.size should be (10004))
  }

  it should "endure 190.000 batch" in {
    Put("/example", (10001 to 200000).map(i => Example(i, s"$i'th element"))) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
      es.state().map(s => s.size should be (10005))
    }
  }

  it should "clear repository" in {
    Delete("/example") ~> routes ~> check {
      response.status should be (StatusCodes.NoContent)
      es.state().map(s => s.size should be (10006))
    }
  }

}
