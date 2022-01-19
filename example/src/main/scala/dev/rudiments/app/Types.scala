package dev.rudiments.app

import com.typesafe.scalalogging.LazyLogging
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.http.ScalaRORouter
import io.circe.{Encoder, Json}

class Types(implicit space: Space) extends LazyLogging {
  implicit val encoder: Encoder[Thing] = encode

  private def encode(thing: Thing): Json = thing match {
    case p: Plain => p match {
      case ScalaTypes.ScalaInt => Json.fromString("int")
      case ScalaTypes.ScalaLong => Json.fromString("long")
      case ScalaTypes.ScalaString => Json.fromString("string")
      case Plain.Bool => Json.fromString("bool")
      case other => Json.fromString(other.toString)
    }
    case t: Type => Json.obj(t.fields.map(f => f.name -> encode(f.p)):_*)
    case a: Abstract => Json.obj(a.fields.map(f => f.name -> encode(f.p)):_*)
    case d: Data => Json.fromString(d.toString)
  }

  private val typesAgent = ID("types").asPath.find
  val router = new ScalaRORouter[Thing](
    Path(ID("types")),
    ScalaTypes.ScalaString,
    typesAgent
  )
}
