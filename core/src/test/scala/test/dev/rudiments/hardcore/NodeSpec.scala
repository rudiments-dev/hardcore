package test.dev.rudiments.hardcore

import dev.rudiments.hardcore._
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NodeSpec extends AnyWordSpec with Matchers {
  case class Something(a: String, i: Int)
  private val s1 = Something("abc", 42)
  private val s2 = Something("cde", 24)

  "Node" should {
    val node: Node = Node.empty
    "created empty" in {
      node.size should be (0)
    }

    "add something" in {
      node >+ ID("42") -> s1 should be (Created(s1))
      node.size should be (1)
    }

    "update something" in {
      node >* ID("42") -> s2 should be(Updated(s1, s2))
      node.size should be(1)
    }

    "delete something" in {
      node >- ID("42") should be(Deleted(s2))
      node.size should be(0)
    }

    "apply commit" in {
      val pairs = (1 to 10).map (i => ID(i.toString) -> Created(Something(i.toHexString, i)))
      node(Commit(pairs:_*)) should be(Applied(Commit(pairs:_*)))
      node.size should be(10)
    }

    "create nested node" in {
      node >+ ID("n") -> Node.empty should be (Created(Node.empty))
      node.size should be(11)
    }

    "put nested values" in {
      val p = ID("n") / ID("123")
      node >+ p -> s1 should be (Created(s1))
      node.size should be (11)
      node.read(p) should be (Readen(s1))
    }

    "apply nested commits" in {
      val pairs = (24 to 42).map (i => ID(i.toString) -> Created(Something(i.toHexString, i)))
      node(Commit(ID("n") -> Commit(pairs:_*))) should be (Applied(
        Commit(ID("n") -> Commit(pairs:_*))
      ))
      node.state(ID("n")).asInstanceOf[Node].size should be (20)
    }
  }
}
