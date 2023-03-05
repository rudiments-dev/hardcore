package test.dev.rudiments.hardcore

import dev.rudiments.hardcore._
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NodeSpec extends AnyWordSpec with Matchers {
  case class Something(a: String, i: Int)
  val s1 = Something("abc", 42)
  val s2 = Something("cde", 24)

  "Node" should {
    val node: Node = Node.empty
    "created empty" in {
      node.size should be (0)
    }

    "can add something" in {
      node >+ ID("42") -> s1 should be (Created(s1))
      node.size should be (1)
    }

    "can update something" in {
      node >* ID("42") -> s2 should be(Updated(s1, s2))
      node.size should be(1)
    }

    "can delete something" in {
      node >- ID("42") should be(Deleted(s2))
      node.size should be(0)
    }
  }
}
