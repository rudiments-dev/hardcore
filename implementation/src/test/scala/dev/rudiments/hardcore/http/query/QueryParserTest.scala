package dev.rudiments.hardcore.http.query

import dev.rudiments.hardcore.http.query.blueprints.{IntLessBlueprint, StringEqualsBlueprint, StartsWith}
import dev.rudiments.hardcore.types.DTO
import org.scalatest.{Matchers, WordSpec}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class QueryParserTest extends WordSpec with Matchers {

  case class Foo(a: Int, b: String) extends DTO

  "simple parse" in {
    val params = HttpParams("b=eq:hi")
    val blueprint = QueryParser.parse[Foo](params)

    val expect = QueryBlueprint[Foo](Set(
      StringEqualsBlueprint("b", "hi")
    ))

    blueprint should be (Right(expect))
  }

  "simple parse 2 params" in {
    val params = HttpParams("b=eq:hi;b=starts:h")
    val blueprint = QueryParser.parse[Foo](params)
    val expect = QueryBlueprint[Foo](Set(
      StringEqualsBlueprint("b", "hi"),
      StartsWith("b", "h")
    ))

    blueprint should be (Right(expect))
  }

  "simple parse 2 field" in {
    val params = HttpParams("b=eq:hi;a=less:3")
    val blueprint = QueryParser.parse[Foo](params)

    val expect = QueryBlueprint[Foo](Set(
      StringEqualsBlueprint("b", "hi"),
      IntLessBlueprint("a", 3)
    ))

    blueprint should be (Right(expect))
  }
}
