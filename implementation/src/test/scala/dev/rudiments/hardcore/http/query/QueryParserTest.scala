package dev.rudiments.hardcore.http.query

import dev.rudiments.hardcore.http.query.predicates.{IntEquals, IntLess, IsDefined, IsEmpty, OptionValuePredicate, ProductFieldPredicate, StartsWith, StringEquals}
import dev.rudiments.hardcore.types.{DTO, Field, FieldFlag, Infinity, NegativeInfinity, NumberFormat, PositiveInfinity, Type, Types}
import org.scalatest.{Matchers, WordSpec}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class QueryParserTest extends WordSpec with Matchers {

  case class Baz(f: Int) extends DTO
  case class Foo(a: Int, b: String, d: Option[Option[Int]], baz: Option[Baz] = Some(Baz(1))) extends DTO

  val bazType: Type = Type("Baz", Map("f" -> Field(Types.Number(NegativeInfinity, PositiveInfinity, NumberFormat.Integer), FieldFlag.Required)))
  val fooType: Type = Type("Foo", Map(
    "a" -> Field(Types.Number(NegativeInfinity, PositiveInfinity, NumberFormat.Integer), FieldFlag.Required),
    "b" -> Field(Types.Text(Infinity), FieldFlag.Required),
    "d" -> Field(Types.Number(NegativeInfinity, PositiveInfinity, NumberFormat.Integer), FieldFlag.Optional),
    "baz" -> Field(Types.Reference(bazType), FieldFlag.Optional),
  ))

  "parse equals expression" in {
    val params = HttpParams("b=eq:hi")
    val query = QueryParser.parse(params, fooType)

    val expect = Query(Set(
      StringEquals("b", "hi")
    ), fooType)

    query should be (Right(expect))
  }

  "parse 2 params on same field" in {
    val params = HttpParams("b=eq:hi;b=starts:h")
    val query = QueryParser.parse(params, fooType)
    val expect = Query(Set(
      StringEquals("b", "hi"),
      StartsWith("b", "h")
    ), fooType)

    query should be (Right(expect))
  }

  "parse 2 params on different fields" in {
    val params = HttpParams("b=eq:hi;a=less:3")
    val query = QueryParser.parse(params, fooType)

    val expect = Query(Set(
      StringEquals("b", "hi"),
      IntLess("a", 3)
    ), fooType)

    query should be (Right(expect))
  }

  "parse option field" in {
    val params = HttpParams("d=eq:3")
    val query = QueryParser.parse(params, fooType)

    val expect = Query(Set(
      OptionValuePredicate(
        "d",
        IntEquals("d", 3)
      )
    ), fooType)

    query should be(Right(expect))
  }

  "parse option is empty" in {
    val params = HttpParams("d=empty")
    val query = QueryParser.parse(params, fooType)

    val expect = Query(Set(
      IsEmpty("d")
    ), fooType)

    query should be (Right(expect))
  }

  "parse option is defined" in {
    val params = HttpParams("d=defined")
    val query = QueryParser.parse(params, fooType)

    val expect = Query(Set(
      IsDefined("d")
    ), fooType)

    query should be (Right(expect))
  }

  "parse object field predicate" in {
    val params = HttpParams("baz.f=eq:1")
    val query = QueryParser.parse(params, fooType)

    val expect = Query(Set(
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
    ), fooType)

    query should be(Right(expect))
  }
}
