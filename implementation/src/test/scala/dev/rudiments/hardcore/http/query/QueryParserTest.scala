package dev.rudiments.hardcore.http.query

import dev.rudiments.domain.Size._
import dev.rudiments.domain._
import dev.rudiments.hardcore.http.query.predicates.{IntEquals, IntLess, IsDefined, IsEmpty, OptionValuePredicate, ProductFieldPredicate, StringEquals, StringStartsWith}
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

import scala.collection.immutable.ListMap

@RunWith(classOf[JUnitRunner])
class QueryParserTest extends AnyWordSpec with Matchers {

  case class Baz(f: Int) extends DTO
  case class Foo(a: Int, b: String, d: Option[Option[Int]], baz: Option[Baz] = Some(Baz(1))) extends DTO

  val domain: Domain = new Domain

  val bazType: Spec = domain.save(
    "Baz",
    Spec(
      "Baz",
      "dev.rudiments.hardcore.http.query.QueryParserTest.Baz",
      ListMap(
        "f" -> ValueSpec(Plain.Number(NegativeInfinity, PositiveInfinity, NumberFormat.Integer), true)
      )
    ),
    Set.empty
  )

  val fooType: Spec = domain.save(
    "Foo",
    Spec(
      "Foo",
      "dev.rudiments.hardcore.http.query.QueryParserTest.Foo",
      ListMap(
        "a" -> ValueSpec(Plain.Number(NegativeInfinity, PositiveInfinity, NumberFormat.Integer), true),
        "b" -> ValueSpec(Plain.Text(Infinity), true),
        "d" -> ValueSpec(Plain.Number(NegativeInfinity, PositiveInfinity, NumberFormat.Integer), false),
        "baz" -> ValueSpec(bazType, false)
      )
    ),
    Set.empty
  )

  "parse equals expression" in {
    val params = HttpParams("b=eq:hi")
    val query = QueryParser.parse(params, fooType)

    val expect = PredicatesQuery(Set(
      StringEquals("b", "hi")
    ), fooType)

    query should be (Right(expect))
  }

  "parse 2 params on same field" in {
    val params = HttpParams("b=eq:hi;b=starts:h")
    val query = QueryParser.parse(params, fooType)
    val expect = PredicatesQuery(Set(
      StringEquals("b", "hi"),
      StringStartsWith("b", "h")
    ), fooType)

    query should be (Right(expect))
  }

  "parse 2 params on different fields" in {
    val params = HttpParams("b=eq:hi;a=less:3")
    val query = QueryParser.parse(params, fooType)

    val expect = PredicatesQuery(Set(
      StringEquals("b", "hi"),
      IntLess("a", 3)
    ), fooType)

    query should be (Right(expect))
  }

  "parse option field" in {
    val params = HttpParams("d=eq:3")
    val query = QueryParser.parse(params, fooType)

    val expect = PredicatesQuery(Set(
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

    val expect = PredicatesQuery(Set(
      IsEmpty("d")
    ), fooType)

    query should be (Right(expect))
  }

  "parse option is defined" in {
    val params = HttpParams("d=defined")
    val query = QueryParser.parse(params, fooType)

    val expect = PredicatesQuery(Set(
      IsDefined("d")
    ), fooType)

    query should be (Right(expect))
  }

  "parse object field predicate" in {
    val params = HttpParams("baz.f=eq:1")
    val query = QueryParser.parse(params, fooType)

    val expect = PredicatesQuery(Set(
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
