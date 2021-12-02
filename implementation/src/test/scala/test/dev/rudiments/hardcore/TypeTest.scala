package test.dev.rudiments.hardcore

import dev.rudiments.hardcore._
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner
import test.dev.rudiments.{Blah, Smt}

@RunWith(classOf[JUnitRunner])
class TypeTest extends AnyWordSpec with Matchers {

  "can build type from case class" in {
    Type.build[Smt] should be (
      Type(
        Seq(
          Field("id", AllOf(ScalaTypes.ScalaLong, Required)),
          Field("name", AllOf(ScalaTypes.ScalaString, Required)),
          Field("comment", ScalaTypes.ScalaString)
        ), Some("test.dev.rudiments.Smt")
      )
    )
  }

  "can build type from trait" in {
    Type.build[Blah] should be (
      Abstract()
    )
  }
}
