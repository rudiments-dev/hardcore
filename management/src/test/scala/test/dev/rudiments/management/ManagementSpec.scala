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
  mem << Management.typesCommit

  "all types should exist" in {
    mem ? (types / "User") should be(Readen(Type(Field("name", Text(1024)), Field("email", Text(1024)))))
    mem ? (types / "Task") should be(Readen(Type(
      Field("name", Text(4096)),
      Field("summary", Text(4 * 1024 * 1024)),
      Field("deadline", Date)
//      Field("assigned", Link(types / "User", Type(Field("name", Text(1024)))))
    )))
  }

  "all locations should exist" in {
    mem << Management.locationsCommit
    mem ? Management.work should be (Readen(Node(
      branches = mutable.Map(
        ID("docs") -> Node.empty,
        ID("team") -> Node(leafIs = mem ! types / "User"),
        ID("tasks") -> Node(leafIs = mem ! types / "Task"),
        ID("meetings") -> Node.empty
      )
    )))
  }
}
