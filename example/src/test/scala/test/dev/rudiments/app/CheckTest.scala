package test.dev.rudiments.app

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CheckTest extends AnyWordSpec with Matchers {
  "always true" in {
    val a = true
    a should be(true)
  }
}
