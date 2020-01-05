package dev.rudiments.hardcore.http.query

import dev.rudiments.hardcore.http.query.blueprints.{IntEqualsBlueprint, IntLessBlueprint, IsDefined, IsEmpty, StartsWith, StringEqualsBlueprint, ValuePredicate}
import dev.rudiments.hardcore.types.DTO
import org.scalatest.{Matchers, WordSpec}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class QueryParserTest extends WordSpec with Matchers {

  case class Foo(a: Int, b: String, d: Option[Int]) extends DTO

  "simple parse" in {
    val params = HttpParams("b=eq:hi")
    val blueprint = QueryParser.parse[Foo](params)

    val expect = QueryBlueprint[Foo](Set(
      StringEqualsBlueprint("b", "hi")
    ))

    blueprint should be (Right(expect))
  }

  "simple parse 2 params" in {
    val params = HttpParams("b=eq:hi;b=starts:h")
    val blueprint = QueryParser.parse[Foo](params)
    val expect = QueryBlueprint[Foo](Set(
      StringEqualsBlueprint("b", "hi"),
      StartsWith("b", "h")
    ))

    blueprint should be (Right(expect))
  }

  "simple parse 2 field" in {
    val params = HttpParams("b=eq:hi;a=less:3")
    val blueprint = QueryParser.parse[Foo](params)

    val expect = QueryBlueprint[Foo](Set(
      StringEqualsBlueprint("b", "hi"),
      IntLessBlueprint("a", 3)
    ))

    blueprint should be (Right(expect))
  }

  "simple parse option field" in {
    val params = HttpParams("d=eq:3")
    val blueprint = QueryParser.parse[Foo](params)

    val expect = QueryBlueprint[Foo](Set(
      ValuePredicate {
        IntEqualsBlueprint("d", 3)
      }
    ))

    blueprint should be (Right(expect))
  }

  "simple parse option is empty" in {
    val params = HttpParams("d=empty")
    val blueprint = QueryParser.parse[Foo](params)

    val expect = QueryBlueprint[Foo](Set(
      IsEmpty("d")
    ))

    blueprint should be (Right(expect))
  }

  "simple parse option is defined" in {
    val params = HttpParams("d=defined")
    val blueprint = QueryParser.parse[Foo](params)

    val expect = QueryBlueprint[Foo](Set(
      IsDefined("d")
    ))

    blueprint should be (Right(expect))
  }
}
