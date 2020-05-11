package dev.rudiments.hardcore.http.query.interop

import dev.rudiments.hardcore.http.query.predicates.{IntEquals, IsDefined, IsEmpty, OptionValuePredicate, ProductFieldPredicate, StringEquals}
import dev.rudiments.hardcore.http.query.{HttpParams, Query, QueryParser}
import dev.rudiments.hardcore.types.DTO
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class InMemoryQueryExecutorTest extends WordSpec with Matchers {

  case class Baz(f: Int) extends DTO
  case class Foo(a: Int, b: String, d: Option[Int] = None, baz: Option[Baz] = None) extends DTO


  "simple query" in {
    val query = Query[Foo](Set(
      StringEquals("b", "hi")
    ))

    val input = Seq(
      Foo(3, "hi"),
      Foo(4, "bay"),
      Foo(5, "tra")
    )

    val result = InMemoryQueryExecutor(query)(input)

    result should be (Seq(
      Foo(3, "hi")
    ))
  }


  "simple query by two field" in {
    val query = Query[Foo](Set(
      StringEquals("b", "bay"),
      IntEquals("a", 5)
    ))

    val input = Seq(
      Foo(3, "hi"),
      Foo(4, "bay"),
      Foo(5, "bay"),
      Foo(6, "tra")
    )

    val result = InMemoryQueryExecutor(query)(input)

    result should be (Seq(
      Foo(5, "bay")
    ))
  }

  "simple query by option field" in {
    val query = Query[Foo](Set(
      OptionValuePredicate("d", IntEquals("d", 1))
    ))

    val input = Seq(
      Foo(3, "hi", Some(1)),
      Foo(4, "bay"),
      Foo(5, "bay"),
      Foo(6, "tra")
    )

    val result = InMemoryQueryExecutor(query)(input)

    result should be (Seq(
      Foo(3, "hi", Some(1)),
    ))
  }

  "simple query by option field, is empty" in {
    val query = Query[Foo](Set(
      IsEmpty("d")
    ))

    val input = Seq(
      Foo(3, "hi", Some(1)),
      Foo(4, "bay"),
      Foo(5, "bay"),
      Foo(6, "tra")
    )

    val result = InMemoryQueryExecutor(query)(input)

    result should be (Seq(
      Foo(4, "bay"),
      Foo(5, "bay"),
      Foo(6, "tra")
    ))
  }

  "simple query by option field, is defined" in {
    val query = Query[Foo](Set(
      IsDefined("d")
    ))

    val input = Seq(
      Foo(3, "hi", Some(1)),
      Foo(4, "bay"),
      Foo(5, "bay"),
      Foo(6, "tra")
    )

    val result = InMemoryQueryExecutor(query)(input)

    result should be (Seq(
      Foo(3, "hi", Some(1))
    ))
  }

  "compile query with object field predicate" in {
    val query = Query[Foo](Set(
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

    val input = Seq(
      Foo(3, "hi", Some(1)),
      Foo(4, "bay", Some(1), Some(Baz(1))),
      Foo(5, "bay"),
      Foo(6, "tra")
    )

    val result = InMemoryQueryExecutor(query)(input)
    result should be (Seq(
      Foo(4, "bay", Some(1), Some(Baz(1))),
    ))
  }

}
