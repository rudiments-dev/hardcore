package dev.rudiments.hardcore.http.query

import dev.rudiments.hardcore.http.query.blueprints.{IntEqualsBlueprint, IntLessBlueprint, IsDefined, IsEmpty, OptionValuePredicate, ProductFieldPredicate, StartsWith, StringEqualsBlueprint}
import dev.rudiments.hardcore.types.DTO
import org.scalatest.{Matchers, WordSpec}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class QueryParserTest extends WordSpec with Matchers {

  case class Baz(f: Int) extends DTO
  case class Foo(a: Int, b: String, d: Option[Option[Int]], baz: Option[Baz] = Some(Baz(1))) extends DTO


  "parse equals expression" in {
    val params = HttpParams("b=eq:hi")
    val blueprint = QueryParser.parse[Foo](params)

    val expect = QueryBlueprint[Foo](Set(
      StringEqualsBlueprint("b", "hi")
    ))

    blueprint should be (Right(expect))
  }

  "parse 2 params on same field" in {
    val params = HttpParams("b=eq:hi;b=starts:h")
    val blueprint = QueryParser.parse[Foo](params)
    val expect = QueryBlueprint[Foo](Set(
      StringEqualsBlueprint("b", "hi"),
      StartsWith("b", "h")
    ))

    blueprint should be (Right(expect))
  }

  "parse 2 params on different fields" in {
    val params = HttpParams("b=eq:hi;a=less:3")
    val blueprint = QueryParser.parse[Foo](params)

    val expect = QueryBlueprint[Foo](Set(
      StringEqualsBlueprint("b", "hi"),
      IntLessBlueprint("a", 3)
    ))

    blueprint should be (Right(expect))
  }

  "parse option field" in {
    val params = HttpParams("d=eq:3")
    val blueprint = QueryParser.parse[Foo](params)

    val expect = QueryBlueprint[Foo](Set(
      OptionValuePredicate(
        "d",
        OptionValuePredicate(
          "d",
          IntEqualsBlueprint("d", 3)
        )
      )
    ))

    blueprint should be (Right(expect))
  }

  "parse option is empty" in {
    val params = HttpParams("d=empty")
    val blueprint = QueryParser.parse[Foo](params)

    val expect = QueryBlueprint[Foo](Set(
      IsEmpty("d")
    ))

    blueprint should be (Right(expect))
  }

  "parse option is defined" in {
    val params = HttpParams("d=defined")
    val blueprint = QueryParser.parse[Foo](params)

    val expect = QueryBlueprint[Foo](Set(
      IsDefined("d")
    ))

    blueprint should be (Right(expect))
  }

  "parse object field predicate" in {
    val params = HttpParams("baz.f=eq:1")
    val blueprint = QueryParser.parse[Foo](params)

    val expect = QueryBlueprint[Foo](Set(
      OptionValuePredicate(
        "baz",
        ProductFieldPredicate(
          "baz",
          IntEqualsBlueprint(
            "f",
            1
          )
        )
      )
    ))

    blueprint should be(Right(expect))
  }
}
