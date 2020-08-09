package dev.rudiments.hardcore.http

import dev.rudiments.types.hard.ScalaTypes
import dev.rudiments.types.{Field, Type, TypeSystem}
import io.circe.Json
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.ListMap

@RunWith(classOf[JUnitRunner])
class SoftEncoderTest extends FlatSpec with Matchers {

  case class Test(f : Option[Double])

  private val typeSystem = TypeSystem()
  private val `type`: Type = Type("Test", ListMap(
    "f" -> Field("f", ScalaTypes.ScalaDouble, false)
  ))

  it should "encode class with option flag" in {
    val encoder = new InstanceEncoder(typeSystem)(`type`)

    val result = encoder.apply(`type`.fromScala(Test(None)))
    result should be(Json.fromFields(List("f" -> Json.Null)))
  }
}
