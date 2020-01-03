package dev.rudiments.hardcore.http.query.interop

import dev.rudiments.hardcore.http.query.blueprints.{IntEqualsBlueprint, StringEqualsBlueprint}
import dev.rudiments.hardcore.http.query.QueryBlueprint
import dev.rudiments.hardcore.types.DTO
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class CompilerTest extends WordSpec with Matchers {

  case class Foo(a: Int, b: String) extends DTO


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
}
