package dev.rudiments.hardcore.http

import dev.rudiments.hardcore.Size._
import dev.rudiments.hardcore._
import io.circe.Json

object ThingEncoder {
  def encode(thing: Thing): Json = thing match {
    case p: Plain => p match {
      case ScalaTypes.ScalaInt => Json.fromString("int")
      case ScalaTypes.ScalaLong => Json.fromString("long")
      case ScalaTypes.ScalaString => Json.fromString("string")
      case Plain.Bool => Json.fromString("bool")
      case Plain.UUID => Json.fromString("uuid")
      case n: Plain.Number => encode(n)
      case other => Json.fromString(other.toString)
    }
    case t: Type => Json.obj(t.fields.map(f => f.name -> encode(f.p)):_*)
    case a: Abstract => Json.obj(a.fields.map(f => f.name -> encode(f.p)):_*)
    case l: List => Json.obj( "type" -> Json.fromString("list"), "of" -> encode(l.item))
    case i: Index => Json.obj("type" -> Json.fromString("index"), "of" -> encode(i.of), "over" -> encode(i.over))
    case Ref(path, _, _) => Json.fromString(path.ids.last.toString)
    case d: Data => encode(d)
    case All => Json.fromString("∀")
  }

  def encode(n: Plain.Number): Json = n match {
    case Plain.Number(min, max, f) => Json.obj(
      "type" -> Json.fromString("Number"),
      "min" -> encode(min),
      "max" -> encode(max),
      "format" -> Json.fromString(f.toString),
    )
  }

  def encode(size: Size): Json = size match {
    case Big(i) => Json.fromString(i.toString())
    case Infinity => Json.fromString("∞")
    case PositiveInfinity => Json.fromString("+∞")
    case NegativeInfinity => Json.fromString("-∞")
  }

  def encode(data: Data): Json = data match {
    case Data(List(of), data: Seq[Any]) => Json.arr(data.map(d => encode(of, d)):_*)
    case Data(Index(of, over), data: Map[_, _]) => Json.obj(
      data.map { case (k, v) => k.toString -> encode(over, v) }.toSeq :_*
    )
    case Data(t: Type, data: Any) => encode(t, data)
    case Data(Ref(_, t: Type, _), data: Any) => encode(t, data)
    case Data(Nothing, Nothing) => Json.fromString("∅") //TODO think
  }

  def encode(predicate: Predicate, value: Any): Json = (predicate, value) match {
    case (t: Type, d: Seq[Any]) => encode(t.fields, d)
    case (l: List, d: Seq[Any]) => Json.arr(d.map(item => encode(l.item, item)):_*)
    case (i: Index, d: Map[_, _]) => Json.obj(d.map { case (k, v) => k.toString -> encode(i.over, v) }.toSeq:_*)
    case (Ref(_, t: Type, _), d: Seq[Any]) => encode(t.fields, d)
    case (Ref(_, t: Abstract, _), d: Seq[Any]) => encode(t.fields, d)
    case (ScalaTypes.ScalaInt, i: Int) => Json.fromInt(i)
    case (ScalaTypes.ScalaLong, l: Long) => Json.fromLong(l)
    case (ScalaTypes.ScalaString, s: String) => Json.fromString(s)
    case (Plain.Bool, b: Boolean) => Json.fromBoolean(b)
    case (t, other) => Json.fromString(t.toString + ":" + other.toString)
  }

  def encode(fields: Seq[Field], values: Seq[Any]): Json = Json.obj(
    fields.zip(values).map { case (field, v) =>
      field.name -> (if(field.required && v != None) encode(field.p, v) else Json.Null)
    } :_*
  )

  def encode(a: Agent)(implicit space: Space): Json = ???
}
