package test.dev.rudiments.utils

import dev.rudiments.utils.SaltedMap
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SaltedMapTest extends AnyWordSpec with Matchers {
  "Salted hash" should {
    "fit for ordered keys" in {
      SaltedMap.salty(0 :: 1 :: 2 :: 3 :: Nil) should be (4)
    }

    "fit for unordered keys" ignore {
      SaltedMap.salty(1 :: 3 :: 0 :: 2 :: Nil) should be(5)
    }
  }
}
