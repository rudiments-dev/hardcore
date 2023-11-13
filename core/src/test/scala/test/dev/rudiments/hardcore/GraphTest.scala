package test.dev.rudiments.hardcore

import dev.rudiments.hardcore.Graph
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GraphTest extends AnyWordSpec with Matchers {

  "can create empty graph" in {
    Graph.empty[Int, Int, Int] should be(Graph[Int, Int, Int](Map.empty, Seq.empty))
  }
}
