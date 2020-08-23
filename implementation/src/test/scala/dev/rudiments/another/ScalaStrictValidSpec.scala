package dev.rudiments.another

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class ScalaStrictValidSpec extends WordSpec with Matchers {
  val ts: TypeSystem = TypeSystem()

  "ScalaStrictValid return Right if arg type compatible" in {
    ScalaStrictValid[String](ts, "1") should be ("1")
    ScalaStrictValid[Int](ts, 1) should be (1)
    ScalaStrictValid[Long](ts, 1L) should be (1L)
    ScalaStrictValid[Double](ts, 1.0) should be (1.0)
  }



  "ScalaStrictValid return Left if arg type incompatible" in {
    assertThrows[IllegalArgumentException] {
      ScalaStrictValid[String](ts, 1.0)
    }
    assertThrows[IllegalArgumentException] {
      ScalaStrictValid[Int](ts, "1")
    }
    assertThrows[IllegalArgumentException] {
      ScalaStrictValid[Long](ts, "1")
    }
    assertThrows[IllegalArgumentException] {
      ScalaStrictValid[Double](ts, "1.0")
    }
  }
}