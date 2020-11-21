package dev.rudiments.hardcore.http.query.interop

import dev.rudiments.domain.ScalaTypes._
import dev.rudiments.hardcore.http.query.PredicatesQuery
import dev.rudiments.hardcore.http.query.predicates._
import dev.rudiments.domain._
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

import scala.collection.immutable.ListMap

@RunWith(classOf[JUnitRunner])
class InMemoryQueryExecutorTest extends AnyWordSpec with Matchers {

  private val domain: Domain = new Domain

  case class Baz(f: Int) extends DTO
  case class Foo(a: Int, b: String, d: Option[Int] = None, baz: Option[Baz] = None) extends DTO

  val bazType: Spec = domain.save(
    "Baz",
    Spec(
      "Baz",
      "dev.rudiments.hardcore.http.query.interop.InMemoryQueryExecutorTest.Baz",
      ListMap("f" -> ValueSpec(ScalaInt, true))
    ),
    Set.empty
  )

  val fooType: Spec = domain.save(
    "Foo",
    Spec(
      "Foo",
      "dev.rudiments.hardcore.http.query.interop.InMemoryQueryExecutorTest.Foo",
      ListMap(
        "a" -> ValueSpec(ScalaInt, true),
        "b" -> ValueSpec(ScalaString, true),
        "d" -> ValueSpec(ScalaInt, false),
        "baz" -> ValueSpec(bazType, false)
      )
    ),
    Set.empty
  )


  "simple query" in {
    val query = PredicatesQuery(Set(
      StringEquals("b", "hi")
    ), fooType)

    val input: Seq[Instance] = Seq(
      Foo(3, "hi"),
      Foo(4, "bay"),
      Foo(5, "tra")
    ).map(fooType.fromProduct(domain, _))

    val result = InMemoryQueryExecutor(query)(input)

    result should be (Seq(
      fooType.fromProduct(domain, Foo(3, "hi"))
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
    ).map(fooType.fromProduct(domain, _))

    val result = InMemoryQueryExecutor(query)(input)

    result should be (Seq(
      Foo(5, "bay")
    ).map(fooType.fromProduct(domain, _)))
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
    ).map(fooType.fromProduct(domain, _))

    val result = InMemoryQueryExecutor(query)(input)

    result should be (Seq(
      Foo(3, "hi", Some(1))
    ).map(fooType.fromProduct(domain, _)))
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
    ).map(fooType.fromProduct(domain, _))

    val result = InMemoryQueryExecutor(query)(input)

    result should be (Seq(
      Foo(4, "bay"),
      Foo(5, "bay"),
      Foo(6, "tra")
    ).map(fooType.fromProduct(domain, _)))
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
    ).map(fooType.fromProduct(domain, _))

    val result = InMemoryQueryExecutor(query)(input)

    result should be (Seq(
      Foo(3, "hi", Some(1))
    ).map(fooType.fromProduct(domain, _)))
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
    ).map(fooType.fromProduct(domain, _))

    val result = InMemoryQueryExecutor(query)(input)
    result should be (Seq(
      Foo(4, "bay", Some(1), Some(Baz(1)))
    ).map(fooType.fromProduct(domain, _)))
  }

}
