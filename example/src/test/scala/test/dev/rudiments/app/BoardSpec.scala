package test.dev.rudiments.app

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import dev.rudiments.hardcore.Initial.types
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.http.{CirceSupport, ScalaRouter, ThingDecoder}
import dev.rudiments.management.Management
import io.circe.{Decoder, Json}
import org.junit.runner.RunWith
import org.scalatest.Inside.inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

import java.sql
import scala.collection.mutable

@RunWith(classOf[JUnitRunner])
class BoardSpec extends AnyWordSpec with Matchers with ScalatestRouteTest with CirceSupport {
  private implicit val actorSystem: ActorSystem = ActorSystem()
  private val mem = new Memory()
  Management.init(mem.node)

  private val n: Node = mem /! Management.boards
  private val router = new ScalaRouter(n)
  private val routes = router.seal()
  private implicit val de: Decoder[Thing] = router.de
  private val nodeDe = new ThingDecoder(new TypeSystem(mem /! types)).anythingDecoder
  private val colType = mem ? (types / "BoardColumn") match {
    case Readen(found: Type) => found
    case other => fail(s"unexpected read of types/BoardColumn: $other")
  }
  private val t = mem ? (types / "Task") match {
    case Readen(found: Type) => found
    case other => fail(s"unexpected read of types/Task: $other")
  }

  { //TODO move init data into files
    val tx = new Tx(mem /! Management.tasks)

    tx += ID("task-1") -> t.data(
      "task-1",
      "summ of task #1",
      sql.Date.valueOf("2022-06-06"),
      Link(types / "TODO", Nothing)
    )
    tx += ID("task-2") -> t.data(
      "task-2",
      "summ of task #2",
      sql.Date.valueOf("2022-05-07"),
      Link(types / "InProgress", Nothing)
    )
    tx += ID("task-3") -> t.data(
      "task-3",
      "summ of task #3",
      sql.Date.valueOf("2022-04-08"),
      Link(types / "Done", Nothing)
    )

    tx.>> match {
      case Prepared(c1) =>
        mem /! Management.tasks << c1 match {
          case Committed(c2) => c2
          case other =>
            fail(s"Failed to commit Tx: $other")
        }
      case other =>
        fail(s"Failed to prepare Tx: $other")
    }
  }

  private val board1: Thing = Node(branches = mutable.Map(
    ID("column-1") -> Node(leafIs = colType),
    ID("column-2") -> Node(leafIs = colType),
    ID("column-3") -> Node(leafIs = colType)
  ))

  private val board2: Thing = Node(branches = mutable.Map(
    ID("column-1") -> Node(leafIs = colType),
    ID("column-2") -> Node(leafIs = colType),
    ID("column-3") -> Node(leafIs = colType)
  ))

  "can encode board" in {
    inside(router.thingEncoder(board1)) { case j: Json =>
      val s = j.toString()
      s
    }
  }

  "can decode board" in {
    val encoded = router.thingEncoder(board1)
    inside (nodeDe.decodeJson(encoded)) {
      case Right(t) =>
        t
      case other => fail("Failed to decode")
    }
  }

//  "create task" in {
//    n ? ID("42") should be (NotExist)
//    Post("/42", sample) ~> routes ~> check {
//      response.status should be (StatusCodes.Created)
//      responseAs[Thing] should be (sample)
//    }
//    n ? ID("42") should be (Readen(sample))
//
//    Get("/42") ~> routes ~> check {
//      response.status should be (StatusCodes.OK)
//      responseAs[Thing] should be (sample)
//    }
//  }
//
//  "update task" in {
//    Put("/42", sample2) ~> routes ~> check {
//      response.status should be (StatusCodes.OK)
//      responseAs[Thing] should be (sample2)
//    }
//    Get("/42") ~> routes ~> check {
//      response.status should be (StatusCodes.OK)
//      responseAs[Thing] should be (sample2)
//    }
//  }
//
//  "can't create second task with same ID" in {
//    Post("/42", sample2) ~> routes ~> check {
//      response.status should be (StatusCodes.Conflict)
//    }
//  }
//
//  "delete task" in {
//    Delete("/42") ~> routes ~> check {
//      response.status should be (StatusCodes.NoContent)
//    }
//    Get("/42") ~> routes ~> check {
//      response.status should be (StatusCodes.NotFound)
//    }
//  }
}
