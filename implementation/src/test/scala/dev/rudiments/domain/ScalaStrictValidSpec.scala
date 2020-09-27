package dev.rudiments.domain

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class ScalaStrictValidSpec extends WordSpec with Matchers {
  val domain: Domain = new Domain

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