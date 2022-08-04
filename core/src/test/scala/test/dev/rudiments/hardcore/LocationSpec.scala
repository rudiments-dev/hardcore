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

  private val paths: Map[Location, Thing] = Map(
    id5 -> Nothing,
    Root -> Nothing,
    id1 / id2 -> Nothing,
    id1 / id3 -> Nothing,
    id1 / id4 -> Nothing,
  )

  "can build hierarchical Node from paths" in {
    Memory.fromMap(paths) should be (Memory(
      Nothing,
      Map(id5 -> Nothing),
      Map(
        id1 -> Memory(
          Nothing,
          Map(
            id2 -> Nothing,
            id3 -> Nothing,
            id4 -> Nothing
          )
        )
      )
    ))
  }
}
