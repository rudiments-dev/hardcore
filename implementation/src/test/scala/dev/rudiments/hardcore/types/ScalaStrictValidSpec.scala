package dev.rudiments.hardcore.types

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class ScalaStrictValidSpec extends WordSpec with Matchers {
  "ScalaStrictValid return Right if arg type compatible" in {
    ScalaStrictValid[String]("1") should be (Right("1"))
    ScalaStrictValid[Int](1) should be (Right(1))
    ScalaStrictValid[Long](1L) should be (Right(1L))
    ScalaStrictValid[Double](1.0) should be (Right(1.0))
  }

  "ScalaStrictValid return Left if arg type incompatible" in {
    ScalaStrictValid[String](1.0) should be (Left(IncompatibleScalaType("java.lang.String", 1.0)))
    ScalaStrictValid[Int]("1") should be (Left(IncompatibleScalaType("scala.Int", "1")))
    ScalaStrictValid[Long]("1") should be (Left(IncompatibleScalaType("scala.Long", "1")))
    ScalaStrictValid[Double]("1.0") should be (Left(IncompatibleScalaType("scala.Double", "1.0")))
  }
}