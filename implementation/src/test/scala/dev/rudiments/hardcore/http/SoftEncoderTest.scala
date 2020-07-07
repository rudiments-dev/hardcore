package dev.rudiments.hardcore.http

import dev.rudiments.hardcore.types.{Field, FieldFlag, ScalaTypes, Type}
import io.circe.{Json, JsonObject}
import io.circe.Json.JObject
import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner

import scala.collection.immutable.ListMap

@RunWith(classOf[JUnitRunner])
class SoftEncoderTest extends FlatSpec with Matchers {

  case class Test(f : Option[Double])

  val `type`: Type = Type("Test", ListMap(
    "f" -> Field(ScalaTypes.ScalaDouble, FieldFlag.Optional)
  ))

  it should "encode class with option flag" in {
    val encoder = SoftEncoder(`type`)

    val result = encoder.apply(`type`.fromScala(Test(None)))
    result should be(Json.fromFields(List("f" -> Json.Null)))
  }
}
