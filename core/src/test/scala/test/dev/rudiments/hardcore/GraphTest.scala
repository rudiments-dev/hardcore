package test.dev.rudiments.hardcore

import dev.rudiments.hardcore.Graph
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GraphTest extends AnyWordSpec with Matchers {

  "can create empty graph" in {
    Graph.empty[Int, Int, Int] should be(Graph[Int, Int, Int](Map.empty, Seq.empty))
  }
}
