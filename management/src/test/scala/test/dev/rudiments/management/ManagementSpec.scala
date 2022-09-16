package test.dev.rudiments.management

import dev.rudiments.hardcore.Initial.types
import dev.rudiments.hardcore._
import dev.rudiments.management.Management
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

import scala.collection.mutable

@RunWith(classOf[JUnitRunner])
class ManagementSpec extends AnyWordSpec with Matchers {
  private val mem: Memory = new Memory()
  Management.init(mem.node)

  "all types should exist" in {
    mem ? (types / "User") should be(Readen(Type(Field("name", Text(1024)), Field("email", Text(1024)))))
    mem ? (types / "Task") should be(Readen(Type(
      Field("name", Text(4096)),
      Field("summary", Text(4 * 1024 * 1024)),
      Field("deadline", Date),
      Field("status", mem ! (types / "TaskStatus"))
//      Field("assigned", Link(types / "User", Type(Field("name", Text(1024)))))
    )))

    val readen = mem ? (types / "BoardColumn")
    val expected = Readen(Type(
      Field("tasks", Enlist(mem ! (types / "Task")))
    ))
    readen should be (expected)
  }

  "all locations should exist" in {
    mem ? Management.work should be (Readen(Node(
      branches = mutable.Map(
        ID("team") -> Node(leafIs = mem ! types / "User"),
        ID("tasks") -> Node(leafIs = mem ! types / "Task"),
        ID("boards") -> Node(leafIs = Nothing), //TODO check node constraint
        ID("docs") -> Node.empty,
        ID("meetings") -> Node.empty
      )
    )))
  }
}
