package test.dev.rudiments.app

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import dev.rudiments.hardcore.Initial.types
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.http.{CirceSupport, ScalaRouter, ThingDecoder}
import dev.rudiments.management.Management
import io.circe.{Decoder, Json}
import org.junit.Ignore

import java.sql
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TasksSpec extends AnyWordSpec with Matchers with ScalatestRouteTest with CirceSupport {
  private implicit val actorSystem: ActorSystem = ActorSystem()
  private val mem = new Memory()
  Management.init(mem.node)
  private val ts = new TypeSystem(mem /! types)
  private val td = new ThingDecoder(ts)

  private val n: Node = mem /! Management.tasks
  private val router = new ScalaRouter(n)(td)
  private val routes = router.seal()
  private val t = mem ? (types / "Task") match {
    case Readen(found: Type) => found
    case other => fail(s"unexpected read of types/Task: $other")
  }
  private implicit val de: Decoder[Thing] = td.decoder(t).map(_.asInstanceOf[Data])

//  mem /! Management.team += ID("alice") -> Management.userLink.data("Alice", "alice@test.org")

  private val sample: Thing = t.data(
    "task-1",
    "summ of task #1",
    sql.Date.valueOf("2022-06-06"),
    Link(types / "InProgress", Nothing)
  )
  private val sample2: Thing = t.data(
    "task-2",
    "summ of task #2",
    sql.Date.valueOf("2022-05-07"),
    Link(types / "TODO", Nothing)
  )

  "can encode task" in {
    router.thingEncoder(sample) should be (Json.obj(
      "name" -> Json.fromString("task-1"),
      "summary" -> Json.fromString("summ of task #1"),
      "deadline" -> Json.fromString("2022-06-06"),
      "status" -> Json.fromString("InProgress")
    ))
  }

  "can decode task" in {
    td.dataTypeDecoder(t).decodeJson(Json.obj(
      "name" -> Json.fromString("task-1"),
      "summary" -> Json.fromString("summ of task #1"),
      "deadline" -> Json.fromString("2022-06-06"),
      "status" -> Json.fromString("InProgress")
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
