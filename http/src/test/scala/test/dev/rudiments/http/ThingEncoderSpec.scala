package test.dev.rudiments.http

import dev.rudiments.hardcore._
import dev.rudiments.hardcore.http.ThingEncoder.encodeNode
import dev.rudiments.hardcore.Initial.types
import dev.rudiments.hardcore.http.{CirceSupport, ScalaRouter, ThingDecoder}
import io.circe.Json
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ThingEncoderSpec extends AnyWordSpec with Matchers with CirceSupport {
  private val ctx: Memory = new Memory()
  private val t = Type(
    Field("id", Number(Long.MinValue, Long.MaxValue)),
    Field("name", Text(Int.MaxValue)),
    Field("comment", Text(Int.MaxValue))
  )

  private val mem = new Memory()
  private val ts = new TypeSystem(mem /! types)
  private val td = new ThingDecoder(ts)
  private val router = new ScalaRouter(mem.node)(td)

  private val initial = ctx ?? ID("commits") match {
    case Found(_, values) => if(values.size == 1) {
      values.head._2 match {
        case c: Commit =>
          c
        case other => fail(s"Expecting commit, got $other")
      }
    } else {
      fail(s"Expecting 1 initial commit, got ${values.size}")
    }
  }

  "can encode links" in {
    router.thingEncoder(ctx ! (types / "Bool")) should be (Json.fromString("Bool"))
    router.thingEncoder(ctx ! (types / "Number")) should be (Json.obj(
      "type" -> Json.fromString("Number")
    ))
  }

  "can encode predicates" in {
    router.thingEncoder(ctx ? (types / "Bool")) should be(Json.obj(
      "type" -> Json.fromString("Nothing")
    ))
    router.thingEncoder(ctx ? (types / "Number")) should be(Json.obj(
      "type" -> Json.fromString("Type"),
      "from" -> Json.obj("type" -> Json.fromString("Anything")),
      "to" -> Json.obj("type" -> Json.fromString("Anything"))
    ))
  }

  "can encode first commit" in {
    router.thingEncoder(initial) should be(Json.obj(
      "type" -> Json.fromString("Commit"),
      "crud" -> encodeNode(Node.fromMap(initial.crud))
      )
    )
  }

  "dataEncoder can encode" in {
    router.thingEncoder(t.data(42, "sample", None)) should be (Json.obj(
      "id" -> Json.fromInt(42),
      "name" -> Json.fromString("sample"),
      "comment" -> Json.Null
    ))
  }
}
