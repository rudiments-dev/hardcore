package test.dev.rudiments.http

import dev.rudiments.hardcore._
import dev.rudiments.hardcore.http.ThingEncoder.encodeMem
import dev.rudiments.hardcore.http.{CirceSupport, ScalaRouter}
import io.circe.{Decoder, Json}
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ThingEncoderSpec extends AnyWordSpec with Matchers with CirceSupport {
  private val ctx: Context = new Context()
  private val t = Type(
    Field("id", Number(Long.MinValue, Long.MaxValue)),
    Field("name", Text(Int.MaxValue)),
    Field("comment", Text(Int.MaxValue))
  )

  private val types = ID("types")
  private val mem: Memory = new Memory(Nothing, leafIs = t)
  private val router = new ScalaRouter(mem)

  private val initial = ctx ? (ID("commits") / ID("-1089338897")) match {
    case Readen(cmt: Commit) => cmt
    case _ => throw new IllegalStateException("Expecting initial commit")
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
      "crud" -> encodeMem(Memory.fromMap(initial.crud))
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
