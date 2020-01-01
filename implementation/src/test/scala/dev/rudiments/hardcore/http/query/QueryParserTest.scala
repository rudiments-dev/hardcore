package dev.rudiments.hardcore.http.query

import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{Matchers, WordSpec}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class QueryParserTest extends WordSpec with Matchers {

  case class Foo(a: Int, b: String)


  "simple parse" in {
    val params = HttpParams("b=eq:hi")
    val blueprint = QueryParser.parse[Foo](params)
    blueprint.parts should be (Set(
      EqualsBlueprint[String]("b", "hi")
    ))
  }

  "simple parse 2 params" in {
    val params = HttpParams("b=eq:hi;b=starts:h")
    val blueprint = QueryParser.parse[Foo](params)
    blueprint.parts should be (Set(
      EqualsBlueprint[String]("b", "hi"),
      StartsWithBlueprint[String]("b", "h")
    ))
  }

  "simple parse 2 queries" in {
    val params = HttpParams("b=eq:hi;b=starts:h")
    val blueprint = QueryParser.parse[Foo](params)
    blueprint.parts should be (Set(
      EqualsBlueprint[String]("b", "hi"),
      StartsWithBlueprint[String]("b", "h")
    ))
  }

  "simple parse 2 field" in {
    val params = HttpParams("b=eq:hi;a=less:3")
    val blueprint = QueryParser.parse[Foo](params)
    blueprint.parts should be (Set(
      EqualsBlueprint[String]("b", "hi"),
      LessBlueprint[Int]("a", 3)
    ))
  }
}
