package work.unformed.hardcore.spec

import org.scalatest.{Matchers, WordSpec}

sealed trait Color
case object Black extends Color
case object White extends Color

class SampleSpec extends WordSpec with Matchers {
  "white is white" in {
    White should be (White)
  }

  "black is black" in {
    Black should not be (White)
  }

  "true is true" in {
    true should be (true)
  }

  "true is not false" in {
    true should not be (false)
  }
}
