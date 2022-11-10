package test.dev.rudiments.app

import akka.actor.ActorSystem
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
  private val ts = new TypeSystem(mem /! types)
  private val td = new ThingDecoder(ts)

  private val n: Node = mem /! Management.boards
  private val router = new ScalaRouter(n)(td)
  private val routes = router.seal()
  private val nodeDe = td.anythingDecoder
  private val c = mem ! (types / "BoardColumn")
  private val t = mem ! (types / "Task")

  private val board1: Thing = Node(leafIs = c, leafs = mutable.Map(
    ID("column-1") -> c.data(Seq.empty),
    ID("column-2") -> c.data(Seq(
      Management.tasks / "task-1",
      Management.tasks / "task-2"
    )),
    ID("column-3") -> c.data(Seq.empty)
  ))

  private val tx = new Tx(mem.node)
  //TODO move init data into files
  tx += Management.tasks / "task-1" -> t.data(
    "task-1",
    "summ of task #1",
    sql.Date.valueOf("2022-06-06"),
    Link(types / "TODO", Nothing)
  )
  tx += Management.tasks / "task-2" -> t.data(
    "task-2",
    "summ of task #2",
    sql.Date.valueOf("2022-05-07"),
    Link(types / "InProgress", Nothing)
  )
  tx += Management.tasks / "task-3" -> t.data(
    "task-3",
    "summ of task #3",
    sql.Date.valueOf("2022-04-08"),
    Link(types / "Done", Nothing)
  )

  tx += Management.boards / "board-1" -> board1

  "can prepare tx" in {
    tx.>> match {
      case Prepared(c1) =>
        mem.node << c1 match {
          case Committed(c2) => c2
          case other =>
            fail(s"Failed to commit Tx: $other")
        }
      case other =>
        fail(s"Failed to prepare Tx: $other")
    }
  }

  "can encode board" in {
    router.thingEncoder(board1) should be(Json.obj(
      "type" -> Json.fromString("Node"),
      "key-is" -> Json.obj(
        "type" -> Json.fromString("Text"),
        "max-size" -> Json.fromString("1024")
      ),
      "leaf-is" -> Json.obj("type" -> Json.fromString("BoardColumn")),
      "leafs" -> Json.obj(
        "column-1" -> Json.obj("tasks" -> Json.arr()),
        "column-2" -> Json.obj("tasks" -> Json.arr(
          Json.fromString((Management.tasks / "task-1").toString),
          Json.fromString((Management.tasks / "task-2").toString),
        )),
        "column-3" -> Json.obj("tasks" -> Json.arr())
      )
    ))
  }

  "can decode location" in {
    val json = Json.obj(
      "type" -> Json.fromString("Path"),
      "ids" -> Json.fromString((Management.tasks / "task-1").toString)
    )

    nodeDe.decodeJson(json) should be (Right(Management.tasks / "task-1"))
  }

  "can decode single column" in {
    val json = Json.obj(
      "type" -> Json.fromString("BoardColumn"),
      "tasks" -> Json.arr(
        Json.fromString((Management.tasks / "task-1").toString),
        Json.fromString((Management.tasks / "task-2").toString)
      )
    )
    nodeDe.decodeJson(json) should be (Right(
      c.data(Seq(Location("work/tasks/task-1"), Location("work/tasks/task-2")))
    ))
  }

  "can decode board" in {
    val encoded = router.thingEncoder(board1)
    val decoded = nodeDe.decodeJson(encoded)
    inside(decoded) {
      case Right(n: Node) =>
        n.self should be (Nothing)
        n.keyIs should be (Text(1024))
        n.leafIs should be (c)
        n.leafs.size should be (3)
        n.leafs.get(ID("column-2")) should be (Some(c.data(Seq(
          Management.tasks / "task-1",
          Management.tasks / "task-2"
        ))))
        n should be (board1)
    }
  }
}
