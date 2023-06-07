package test.dev.rudiments.codecs

import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

import io.circe.generic.semiauto.*
import io.circe.{Codec, Json}

@RunWith(classOf[JUnitRunner])
class CirceTest extends AnyWordSpec with Matchers {
  case class Sample(
    a: Int,
    b: String,
    c: Seq[String]
  )

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
