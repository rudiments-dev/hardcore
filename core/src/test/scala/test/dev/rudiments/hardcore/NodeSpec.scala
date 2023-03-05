package test.dev.rudiments.hardcore

import dev.rudiments.hardcore.Node
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NodeSpec extends AnyWordSpec with Matchers {
  "Node" should {
    "created empty" in {
      Node.empty.state.size should be (0)
    }
  }
}
