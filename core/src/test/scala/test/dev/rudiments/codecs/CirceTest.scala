package test.dev.rudiments.codecs

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.circe.generic.semiauto.*
import io.circe.{ Codec, Json }
import test.dev.rudiments.Sample

class CirceTest extends AnyWordSpec with Matchers {
  implicit val codec: Codec[Sample] = Codec.from(deriveDecoder[Sample], deriveEncoder[Sample])
  private val sample = Sample(42, "the answer", Seq("to", "life", "the", "universe", "and", "everything"))

  "can encode custom class" in {
    codec(sample) should be (Json.obj(
      "a" -> Json.fromInt(42),
      "b" -> Json.fromString("the answer"),
      "c" -> Json.arr(
        Json.fromString("to"),
        Json.fromString("life"),
        Json.fromString("the"),
        Json.fromString("universe"),
        Json.fromString("and"),
        Json.fromString("everything")
      )
    ))
  }
}
