package dev.rudiments.app

import com.typesafe.scalalogging.LazyLogging
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.http.ScalaRORouter
import io.circe.{Encoder, Json}

class Types(implicit space: Space) extends LazyLogging {
  implicit val encoder: Encoder[Thing] = encode

  private def encode(predicate: Predicate, data: Any): Json = (predicate, data) match {
    case (t: Type, d: Seq[Any]) =>
      Json.obj(
        t.fields.zip(d).map { case (field, v) => field.name -> encode(field.p, v) }:_*
      )
    case (Ref(_, t: Type, _), d: Seq[Any]) =>
      Json.obj(
        t.fields.zip(d).map { case (field, v) => field.name -> encode(field.p, v) }:_*
      )
    case (Ref(_, t: Abstract, _), d: Seq[Any]) =>
      Json.obj(
        t.fields.zip(d).map { case (field, v) => field.name -> encode(field.p, v) }:_*
      )
    case (ScalaTypes.ScalaInt, i: Int) => Json.fromInt(i)
    case (ScalaTypes.ScalaLong, l: Long) => Json.fromLong(l)
    case (ScalaTypes.ScalaString, s: String) => Json.fromString(s)
    case (Plain.Bool, b: Boolean) => Json.fromBoolean(b)
    case (t, other) => Json.fromString(t.toString + ":" + other.toString)
  }

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
    case Ref(_, t: Type, data: Option[Any]) => data.map(d => encode(t, d)).getOrElse(Json.obj())
    case Ref(_, t: Abstract, data: Option[Any]) => data.map(d => encode(t, d)).getOrElse(Json.obj())
    case Data(t: Type, data: Any) => encode(t, data)
    case Data(Nothing, Nothing) => Json.fromString("∅") //TODO think
  }

  private val typesAgent = ID("types").asPath.find
  val router = new ScalaRORouter[Thing](
    Path(ID("types")),
    ScalaTypes.ScalaString,
    typesAgent
  )
}
