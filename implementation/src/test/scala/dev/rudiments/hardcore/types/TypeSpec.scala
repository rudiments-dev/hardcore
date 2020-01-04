package dev.rudiments.hardcore.types

import org.junit.runner.RunWith
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TypeSpec extends WordSpec with Matchers {
  val t: HardType[Sample1] = HardType[Sample1]

  "can construct map with values" in {
    val m = t.constructMap(1, None, Set.empty)
    m should be (Map("a" -> 1, "b" -> None, "c" -> Set.empty))
    m.head should be ("a" -> 1)
    m.last should be ("c" -> Set.empty)
  }

  "can construct instance of HardType as T" in {
    t.construct(1, None, Set.empty) should be (Sample1(1, None, Set.empty))
  }
}