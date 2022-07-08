package test.dev.rudiments.hardcore

import dev.rudiments.hardcore._
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class LocationSpec extends AnyWordSpec with Matchers {
  private val id1 = ID("42")
  private val id2 = ID("35")
  private val id3 = ID("57")
  private val id4 = ID("13")
  private val id5 = ID("5")

  private val p12 = id1 / id2
  private val p13 = id1 / id3
  private val p14 = id1 / id4

  private val paths: Map[Location, Thing] = Map(
    id5 -> Nothing,
    p12 -> Nothing,
    p13 -> Nothing,
    p14 -> Nothing,
  )

  "can build hierarchical Node from paths" in {
    Node.fromMap(paths) should be (Node[Thing](
      Map(id5 -> Nothing),
      Map(
        id1 -> Node[Thing](
          Map(
            id2 -> Nothing,
            id3 -> Nothing,
            id4 -> Nothing
          ),
          Map.empty
        )
      )
    ))
  }
}
