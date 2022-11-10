package test.dev.rudiments.http

import dev.rudiments.hardcore.Initial.types
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.http.{CirceSupport, ScalaRouter, ThingDecoder}
import io.circe.{Decoder, Json}
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ThingDecoderSpec extends AnyWordSpec with Matchers with CirceSupport {
  private val t = Type(
    Field("id", Number(Long.MinValue, Long.MaxValue)),
    Field("name", Text(Int.MaxValue)),
    Field("comment", Text(Int.MaxValue))
  )

  private val mem = new Memory()
  private val ts = new TypeSystem(mem /! types)
  private val td = new ThingDecoder(ts)
  private val router = new ScalaRouter(mem.node)(td)
  private val de: Decoder[Data] = td.dataTypeDecoder(t)

  "data decoder can decode" in {
    de.decodeJson(Json.obj(
      "id" -> Json.fromInt(42),
      "name" -> Json.fromString("sample"),
      "comment" -> Json.fromString("non-optional comment")
    )) should be (Right(t.data(42, "sample", "non-optional comment")))
  }

  "can encode and decode empty Node" ignore {
    val encoded = thingEncoder(Node.empty)
    encoded should be (Json.obj(
      "type" -> Json.fromString("Node"),
      "self" -> Json.obj("type" -> Json.fromString("Nothing")),
      "leaf-is" -> Json.obj("type" -> Json.fromString("Anything")),
      "key-is" -> Json.obj(
        "type" -> Json.fromString("Text"),
        "maxSize" -> Json.fromString("1024")
      )
    ))

    val decoded = de.decodeJson(encoded)
    decoded should be (Node.empty)
  }
}
