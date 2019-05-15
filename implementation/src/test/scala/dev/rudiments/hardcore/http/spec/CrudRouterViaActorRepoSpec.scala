package dev.rudiments.hardcore.http.spec

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import dev.rudiments.hardcore.dsl.ID._
import dev.rudiments.hardcore.dsl._
import dev.rudiments.hardcore.eventstore.ActorMemory
import dev.rudiments.hardcore.http.CirceSupport._
import dev.rudiments.hardcore.http.{CrudRouter, IDPath}
import dev.rudiments.hardcore.repo.memory.MemoryRepo
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class CrudRouterViaActorRepoSpec extends WordSpec with Matchers with ScalatestRouteTest {
  private case class Example(
    id: Long,
    name: String
  )

  private implicit val actorSystem: ActorSystem = ActorSystem()
  private implicit val meta: Meta[Example] = Meta(value => ID(value.id))
  private val repo: MemoryRepo[Example] = new MemoryRepo[Example]
  private implicit val es: Memory with SkillSet = new ActorMemory
  private val skill = es.withSkill(repo.handle)
  private val router: CrudRouter[Example] = new CrudRouter[Example]("example", skill, IDPath[Example, Long])
  private val routes = Route.seal(router.routes)
  private val sample = Example(42, "sample")
  private val id = sample.identify

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

  "same command returns same result" in {
    Post("/example", sample) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
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
    repo.count().unsafeRunSync() should be (10000)
  }

  "endure 190.000 batch" in {
    Put("/example", (10001 to 200000).map(i => Example(i, s"$i'th element"))) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
      repo.count().unsafeRunSync() should be (200000)
    }
  }

  "clear repository" in {
    Delete("/example") ~> routes ~> check {
      response.status should be (StatusCodes.NoContent)
      repo.count().unsafeRunSync() should be (0)
    }
  }

}
