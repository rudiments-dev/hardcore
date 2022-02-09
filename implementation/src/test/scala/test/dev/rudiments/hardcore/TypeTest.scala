package test.dev.rudiments.hardcore

import dev.rudiments.hardcore.Predicate.AnyWhere
import dev.rudiments.hardcore._
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner
import test.dev.rudiments.{Blah, Smt}

@RunWith(classOf[JUnitRunner])
class TypeTest extends AnyWordSpec with Matchers {
  private implicit val space: Space = new Space()

  "can build type from case class" in {
    Type.build[Smt] should be (
      Ref(
        ID("types") / ID("Smt"),
        Type(
          Seq(
            Field("id", ScalaTypes.ScalaLong, required = true),
            Field("name", ScalaTypes.ScalaString, required = true),
            Field("comment", ScalaTypes.ScalaString, required = false)
          ), Some("test.dev.rudiments.Smt")
        )
      )
    )
  }

  "can build type from trait" in {
    Type.build[Blah] should be (
      Ref(
        ID("types") / ID("Blah"),
        Abstract(Seq.empty, Some("test.dev.rudiments.Blah"))
      )
    )
  }

  "relation between types" in {
    val b = Type.relations >> ID("Blah")
    val s = Type.relations >> ID("Smt")
    val t = Type.relations >> ID("Thng")

    Type.relations >> ID("Blah") should be (
      Readen(ID("Blah"), Data(List(AnyWhere), Set(Path("types/ADT"))))
    )
    Type.relations >> ID("Smt") should be (
      Readen(ID("Smt"), Data(List(AnyWhere), Set(Path("types/ADT"), Path("types/Blah"))))
    )
    Type.relations >> ID("Thng") should be (
      Readen(ID("Thng"), Data(List(AnyWhere), Set(Path("types/ADT"), Path("types/Blah"))))
    )
  }
}
