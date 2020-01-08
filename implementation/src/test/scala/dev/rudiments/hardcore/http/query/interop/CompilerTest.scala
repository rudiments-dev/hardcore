package dev.rudiments.hardcore.http.query.interop

import dev.rudiments.hardcore.http.query.blueprints.{IntEqualsBlueprint, IsDefined, IsEmpty, OptionValuePredicate, ProductFieldPredicate, StringEqualsBlueprint}
import dev.rudiments.hardcore.http.query.{HttpParams, QueryBlueprint, QueryParser}
import dev.rudiments.hardcore.types.DTO
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class CompilerTest extends WordSpec with Matchers {

  case class Baz(f: Int) extends DTO
  case class Foo(a: Int, b: String, d: Option[Int] = None, baz: Option[Baz] = None) extends DTO


  "simple query" in {
    val queryBlueprint = QueryBlueprint[Foo](Set(
      StringEqualsBlueprint("b", "hi")
    ))

    val input = Seq(
      Foo(3, "hi"),
      Foo(4, "bay"),
      Foo(5, "tra")
    )

    val query = Compiler.compile(queryBlueprint)

    val result = input.flatMap(foo => query.testFunction(foo))

    result should be (Seq(
      Foo(3, "hi")
    ))
  }


  "simple query by two field" in {
    val queryBlueprint = QueryBlueprint[Foo](Set(
      StringEqualsBlueprint("b", "bay"),
      IntEqualsBlueprint("a", 5)
    ))

    val input = Seq(
      Foo(3, "hi"),
      Foo(4, "bay"),
      Foo(5, "bay"),
      Foo(6, "tra")
    )

    val query = Compiler.compile(queryBlueprint)

    val result = input.flatMap(foo => query.testFunction(foo))

    result should be (Seq(
      Foo(5, "bay")
    ))
  }

  "simple query by option field" in {
    val queryBlueprint = QueryBlueprint[Foo](Set(
      OptionValuePredicate("d", IntEqualsBlueprint("d", 1))
    ))

    val input = Seq(
      Foo(3, "hi", Some(1)),
      Foo(4, "bay"),
      Foo(5, "bay"),
      Foo(6, "tra")
    )

    val query = Compiler.compile(queryBlueprint)

    val result = input.flatMap(foo => query.testFunction(foo))

    result should be (Seq(
      Foo(3, "hi", Some(1)),
    ))
  }

  "simple query by option field, is empty" in {
    val queryBlueprint = QueryBlueprint[Foo](Set(
      IsEmpty("d")
    ))

    val input = Seq(
      Foo(3, "hi", Some(1)),
      Foo(4, "bay"),
      Foo(5, "bay"),
      Foo(6, "tra")
    )

    val query = Compiler.compile(queryBlueprint)

    val result = input.flatMap(foo => query.testFunction(foo))

    result should be (Seq(
      Foo(4, "bay"),
      Foo(5, "bay"),
      Foo(6, "tra")
    ))
  }

  "simple query by option field, is defined" in {
    val queryBlueprint = QueryBlueprint[Foo](Set(
      IsDefined("d")
    ))

    val input = Seq(
      Foo(3, "hi", Some(1)),
      Foo(4, "bay"),
      Foo(5, "bay"),
      Foo(6, "tra")
    )

    val query = Compiler.compile(queryBlueprint)

    val result = input.flatMap(foo => query.testFunction(foo))

    result should be (Seq(
      Foo(3, "hi", Some(1))
    ))
  }

  "compile query with object field predicate" in {
    val blueprint = QueryBlueprint[Foo](Set(
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

    val input = Seq(
      Foo(3, "hi", Some(1)),
      Foo(4, "bay", Some(1), Some(Baz(1))),
      Foo(5, "bay"),
      Foo(6, "tra")
    )

    val query = Compiler.compile(blueprint)

    val result = input.flatMap(foo => query.testFunction(foo))
    result should be (Seq(
      Foo(4, "bay", Some(1), Some(Baz(1))),
    ))
  }

}
