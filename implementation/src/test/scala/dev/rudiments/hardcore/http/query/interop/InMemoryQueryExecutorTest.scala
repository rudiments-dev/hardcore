package dev.rudiments.hardcore.http.query.interop

import dev.rudiments.hardcore.http.query.PredicatesQuery
import dev.rudiments.hardcore.http.query.predicates._
import dev.rudiments.types._
import dev.rudiments.types.hard.ScalaTypes.{ScalaInt, ScalaString}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class InMemoryQueryExecutorTest extends WordSpec with Matchers {

  case class Baz(f: Int) extends DTO
  case class Foo(a: Int, b: String, d: Option[Int] = None, baz: Option[Baz] = None) extends DTO

  val bazType: Type = Type("Baz", Map(
    "f" -> ValueSpec(ScalaInt, true)
  ))
  val fooType: Type = Type("Foo", Map(
    "a" -> ValueSpec(ScalaInt, true),
    "b" -> ValueSpec(ScalaString, true),
    "d" -> ValueSpec(ScalaInt, false),
    "baz" -> ValueSpec(bazType, false),
  ))


  "simple query" in {
    val query = PredicatesQuery(Set(
      StringEquals("b", "hi")
    ), fooType)

    val input: Seq[Instance] = Seq(
      Foo(3, "hi"),
      Foo(4, "bay"),
      Foo(5, "tra")
    ).map(fooType.fromScala)

    val result = InMemoryQueryExecutor(query)(input)

    result should be (Seq(
      fooType.fromScala(Foo(3, "hi"))
    ))
  }


  "simple query by two field" in {
    val query = PredicatesQuery(Set(
      StringEquals("b", "bay"),
      IntEquals("a", 5)
    ), fooType)

    val input = Seq(
      Foo(3, "hi"),
      Foo(4, "bay"),
      Foo(5, "bay"),
      Foo(6, "tra")
    ).map(fooType.fromScala)

    val result = InMemoryQueryExecutor(query)(input)

    result should be (Seq(
      Foo(5, "bay")
    ).map(fooType.fromScala))
  }

  "simple query by option field" in {
    val query = PredicatesQuery(Set(
      OptionValuePredicate("d", IntEquals("d", 1))
    ), fooType)

    val input = Seq(
      Foo(3, "hi", Some(1)),
      Foo(4, "bay"),
      Foo(5, "bay"),
      Foo(6, "tra")
    ).map(fooType.fromScala)

    val result = InMemoryQueryExecutor(query)(input)

    result should be (Seq(
      Foo(3, "hi", Some(1)),
    ).map(fooType.fromScala))
  }

  "simple query by option field, is empty" in {
    val query = PredicatesQuery(Set(
      IsEmpty("d")
    ), fooType)

    val input = Seq(
      Foo(3, "hi", Some(1)),
      Foo(4, "bay"),
      Foo(5, "bay"),
      Foo(6, "tra")
    ).map(fooType.fromScala)

    val result = InMemoryQueryExecutor(query)(input)

    result should be (Seq(
      Foo(4, "bay"),
      Foo(5, "bay"),
      Foo(6, "tra")
    ).map(fooType.fromScala))
  }

  "simple query by option field, is defined" in {
    val query = PredicatesQuery(Set(
      IsDefined("d")
    ), fooType)

    val input = Seq(
      Foo(3, "hi", Some(1)),
      Foo(4, "bay"),
      Foo(5, "bay"),
      Foo(6, "tra")
    ).map(fooType.fromScala)

    val result = InMemoryQueryExecutor(query)(input)

    result should be (Seq(
      Foo(3, "hi", Some(1))
    ).map(fooType.fromScala))
  }

  "compile query with object field predicate" in {
    val query = PredicatesQuery(Set(
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
    ).map(fooType.fromScala)

    val result = InMemoryQueryExecutor(query)(input)
    result should be (Seq(
      Foo(4, "bay", Some(1), Some(Baz(1))),
    ).map(fooType.fromScala))
  }

}
