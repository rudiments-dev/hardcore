package dev.rudiments.hardcore.http.query.interop

import dev.rudiments.hardcore.http.query.predicates.{IntEquals, IsDefined, IsEmpty, OptionValuePredicate, ProductFieldPredicate, StringEquals}
import dev.rudiments.hardcore.http.query.{HttpParams, Query, QueryParser}
import dev.rudiments.hardcore.types.{DTO, Field, FieldFlag, Infinity, NegativeInfinity, NumberFormat, PositiveInfinity, SoftInstance, Type, Types}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class InMemoryQueryExecutorTest extends WordSpec with Matchers {

  case class Baz(f: Int) extends DTO
  case class Foo(a: Int, b: String, d: Option[Int] = None, baz: Option[Baz] = None) extends DTO

  val bazType: Type = Type("Baz", Map(
    "f" -> Field(Types.Number(NegativeInfinity, PositiveInfinity, NumberFormat.Integer), FieldFlag.Required)
  ))
  val fooType: Type = Type("Foo", Map(
    "a" -> Field(Types.Number(NegativeInfinity, PositiveInfinity, NumberFormat.Integer), FieldFlag.Required),
    "b" -> Field(Types.Text(Infinity), FieldFlag.Required),
    "d" -> Field(Types.Number(NegativeInfinity, PositiveInfinity, NumberFormat.Integer), FieldFlag.Optional),
    "baz" -> Field(Types.Reference(bazType), FieldFlag.Optional),
  ))


  "simple query" in {
    val query = Query(Set(
      StringEquals("b", "hi")
    ), fooType)

    val input: Seq[SoftInstance] = Seq(
      Foo(3, "hi"),
      Foo(4, "bay"),
      Foo(5, "tra")
    ).map(fooType.softFromHard)

    val result = InMemoryQueryExecutor(query)(input)

    result should be (Seq(
      fooType.softFromHard(Foo(3, "hi"))
    ))
  }


  "simple query by two field" in {
    val query = Query(Set(
      StringEquals("b", "bay"),
      IntEquals("a", 5)
    ), fooType)

    val input = Seq(
      Foo(3, "hi"),
      Foo(4, "bay"),
      Foo(5, "bay"),
      Foo(6, "tra")
    ).map(fooType.softFromHard)

    val result = InMemoryQueryExecutor(query)(input)

    result should be (Seq(
      Foo(5, "bay")
    ).map(fooType.softFromHard))
  }

  "simple query by option field" in {
    val query = Query(Set(
      OptionValuePredicate("d", IntEquals("d", 1))
    ), fooType)

    val input = Seq(
      Foo(3, "hi", Some(1)),
      Foo(4, "bay"),
      Foo(5, "bay"),
      Foo(6, "tra")
    ).map(fooType.softFromHard)

    val result = InMemoryQueryExecutor(query)(input)

    result should be (Seq(
      Foo(3, "hi", Some(1)),
    ).map(fooType.softFromHard))
  }

  "simple query by option field, is empty" in {
    val query = Query(Set(
      IsEmpty("d")
    ), fooType)

    val input = Seq(
      Foo(3, "hi", Some(1)),
      Foo(4, "bay"),
      Foo(5, "bay"),
      Foo(6, "tra")
    ).map(fooType.softFromHard)

    val result = InMemoryQueryExecutor(query)(input)

    result should be (Seq(
      Foo(4, "bay"),
      Foo(5, "bay"),
      Foo(6, "tra")
    ).map(fooType.softFromHard))
  }

  "simple query by option field, is defined" in {
    val query = Query(Set(
      IsDefined("d")
    ), fooType)

    val input = Seq(
      Foo(3, "hi", Some(1)),
      Foo(4, "bay"),
      Foo(5, "bay"),
      Foo(6, "tra")
    ).map(fooType.softFromHard)

    val result = InMemoryQueryExecutor(query)(input)

    result should be (Seq(
      Foo(3, "hi", Some(1))
    ).map(fooType.softFromHard))
  }

  "compile query with object field predicate" in {
    val query = Query(Set(
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

    val input = Seq(
      Foo(3, "hi", Some(1)),
      Foo(4, "bay", Some(1), Some(Baz(1))),
      Foo(5, "bay"),
      Foo(6, "tra")
    ).map(fooType.softFromHard)

    val result = InMemoryQueryExecutor(query)(input)
    result should be (Seq(
      Foo(4, "bay", Some(1), Some(Baz(1))),
    ).map(fooType.softFromHard))
  }

}
