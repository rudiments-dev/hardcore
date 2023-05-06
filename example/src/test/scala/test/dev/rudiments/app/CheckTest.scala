package test.dev.rudiments.app

import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CheckTest extends AnyWordSpec with Matchers {
  "always true" in {
    val a = true
    a should be(true)
  }
}
