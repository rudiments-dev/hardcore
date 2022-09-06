package test.dev.rudiments.app

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.http.{CirceSupport, ScalaRouter}
import dev.rudiments.management.Management
import io.circe.{Decoder, Json}

import java.sql
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TasksSpec extends AnyWordSpec with Matchers with ScalatestRouteTest with CirceSupport {
  private implicit val actorSystem: ActorSystem = ActorSystem()
  private val mem = new Memory()
  mem << Management.typesCommit
  mem << Management.locationsCommit

  private val n: Node = mem /! Management.tasks
  private val router = new ScalaRouter(n)
  private val routes = router.seal()
  private implicit val de: Decoder[Thing] = router.de
  private val t = Management.taskType

  mem /! Management.team += ID("alice") -> Management.userLink.data("Alice", "alice@test.org")

  private val sample: Thing = t.data(
    "task-1", "summ of task #1", sql.Date.valueOf("2022-06-06")
  )
  private val sample2: Thing = t.data(
    "task-2", "summ of task #2", sql.Date.valueOf("2022-05-07")
  )

  "can encode task" in {
    router.thingEncoder(sample) should be (Json.obj(
      "name" -> Json.fromString("task-1"),
      "summary" -> Json.fromString("summ of task #1"),
      "deadline" -> Json.fromString("2022-06-06")
    ))
  }

  "can decode task" in {
    router.de.decodeJson(Json.obj(
      "name" -> Json.fromString("task-1"),
      "summary" -> Json.fromString("summ of task #1"),
      "deadline" -> Json.fromString("2022-06-06")
    )) should be (Right(sample))
  }

  "create task" in {
    n ? ID("42") should be (NotExist)
    Post("/42", sample) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
      responseAs[Thing] should be (sample)
    }
    n ? ID("42") should be (Readen(sample))

    Get("/42") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Thing] should be (sample)
    }
  }

  "update task" in {
    Put("/42", sample2) ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Thing] should be (sample2)
    }
    Get("/42") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Thing] should be (sample2)
    }
  }

  "can't create second task with same ID" in {
    Post("/42", sample2) ~> routes ~> check {
      response.status should be (StatusCodes.Conflict)
    }
  }

  "delete task" in {
    Delete("/42") ~> routes ~> check {
      response.status should be (StatusCodes.NoContent)
    }
    Get("/42") ~> routes ~> check {
      response.status should be (StatusCodes.NotFound)
    }
  }
}
