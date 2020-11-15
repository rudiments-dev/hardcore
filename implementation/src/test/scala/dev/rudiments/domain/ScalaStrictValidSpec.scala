package dev.rudiments.domain

import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ScalaStrictValidSpec extends AnyWordSpec with Matchers {
  val domain: Domain = Domain()

  "ScalaStrictValid return Right if arg type compatible" in {
    ScalaStrictValid[String](domain, "1") should be ("1")
    ScalaStrictValid[Int](domain, 1) should be (1)
    ScalaStrictValid[Long](domain, 1L) should be (1L)
    ScalaStrictValid[Double](domain, 1.0) should be (1.0)
  }



  "ScalaStrictValid return Left if arg type incompatible" in {
    assertThrows[IllegalArgumentException] {
      ScalaStrictValid[String](domain, 1.0)
    }
    assertThrows[IllegalArgumentException] {
      ScalaStrictValid[Int](domain, "1")
    }
    assertThrows[IllegalArgumentException] {
      ScalaStrictValid[Long](domain, "1")
    }
    assertThrows[IllegalArgumentException] {
      ScalaStrictValid[Double](domain, "1.0")
    }
  }
}