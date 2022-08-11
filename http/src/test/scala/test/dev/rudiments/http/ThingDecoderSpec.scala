package test.dev.rudiments.http

import dev.rudiments.hardcore._
import dev.rudiments.hardcore.http.{CirceSupport, ScalaRouter}
import io.circe.Json
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

  private val mem: Node = new Node(Nothing, leafIs = t)
  private val router = new ScalaRouter(mem)

  "data decoder can decode" in {
    router.de.decodeJson(Json.obj(
      "id" -> Json.fromInt(42),
      "name" -> Json.fromString("sample"),
      "comment" -> Json.fromString("non-optional comment")
    )) should be (Right(t.data(42, "sample", "non-optional comment")))
  }
}
