package dev.rudiments.hardcore.http.query

import dev.rudiments.hardcore.http.query.predicates.{IntEquals, IntLess, IsDefined, IsEmpty, OptionValuePredicate, ProductFieldPredicate, StartsWith, StringEquals}
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
    val query = QueryParser.parse[Foo](params)

    val expect = Query[Foo](Set(
      StringEquals("b", "hi")
    ))

    query should be (Right(expect))
  }

  "parse 2 params on same field" in {
    val params = HttpParams("b=eq:hi;b=starts:h")
    val query = QueryParser.parse[Foo](params)
    val expect = Query[Foo](Set(
      StringEquals("b", "hi"),
      StartsWith("b", "h")
    ))

    query should be (Right(expect))
  }

  "parse 2 params on different fields" in {
    val params = HttpParams("b=eq:hi;a=less:3")
    val query = QueryParser.parse[Foo](params)

    val expect = Query[Foo](Set(
      StringEquals("b", "hi"),
      IntLess("a", 3)
    ))

    query should be (Right(expect))
  }

  "parse option field" in {
    val params = HttpParams("d=eq:3")
    val query = QueryParser.parse[Foo](params)

    val expect = Query[Foo](Set(
      OptionValuePredicate(
        "d",
        OptionValuePredicate(
          "d",
          IntEquals("d", 3)
        )
      )
    ))

    query should be (Right(expect))
  }

  "parse option is empty" in {
    val params = HttpParams("d=empty")
    val query = QueryParser.parse[Foo](params)

    val expect = Query[Foo](Set(
      IsEmpty("d")
    ))

    query should be (Right(expect))
  }

  "parse option is defined" in {
    val params = HttpParams("d=defined")
    val query = QueryParser.parse[Foo](params)

    val expect = Query[Foo](Set(
      IsDefined("d")
    ))

    query should be (Right(expect))
  }

  "parse object field predicate" in {
    val params = HttpParams("baz.f=eq:1")
    val query = QueryParser.parse[Foo](params)

    val expect = Query[Foo](Set(
      OptionValuePredicate(
        "baz",
        ProductFieldPredicate(
          "baz",
          IntEquals(
            "f",
            1
          )
        )
      )
    ))

    query should be(Right(expect))
  }
}
