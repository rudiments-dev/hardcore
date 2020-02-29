package dev.rudiments.types.registry.module

import dev.rudiments.hardcore.types._
import io.circe.Decoder.Result
import io.circe._

object FieldFormat {
  implicit val fieldEncoder: Encoder[Field] = new Encoder[Field] {

    private def fieldEncoder(field: Field): String = (field match {
      case Field(RudimentTypes.Number(min, max, format), _) => fieldTypeEncoder(RudimentTypes.Number(min, max, format))
      case Field(RudimentTypes.Text(size), _) => fieldTypeEncoder(RudimentTypes.Text(size))

      case Field(RudimentTypes.Date, _) => fieldTypeEncoder(RudimentTypes.Date)
      case Field(RudimentTypes.Time, _) => fieldTypeEncoder(RudimentTypes.Time)
      case Field(RudimentTypes.Timestamp, _) => fieldTypeEncoder(RudimentTypes.Timestamp)

      case Field(RudimentTypes.Enum(n, v), _) => fieldTypeEncoder(RudimentTypes.Enum(n, v))

      case Field(RudimentTypes.Set(t),      FieldFlag.CanBeEmpty) =>  fieldTypeEncoder(t) + "{}"
      case Field(RudimentTypes.Set(t),      FieldFlag.NonEmpty) =>    fieldTypeEncoder(t) + "{!}"
      case Field(RudimentTypes.List(t),     FieldFlag.CanBeEmpty) =>  fieldTypeEncoder(t) + "[]"
      case Field(RudimentTypes.List(t),     FieldFlag.NonEmpty) =>    fieldTypeEncoder(t) + "[!]"
      case Field(RudimentTypes.Index(k, v), FieldFlag.CanBeEmpty) =>  fieldTypeEncoder(k) + "->" + fieldTypeEncoder(v)
      case Field(RudimentTypes.Index(k, v), FieldFlag.NonEmpty) =>    fieldTypeEncoder(k) + "->" + fieldTypeEncoder(v) + "!"

      case Field(RudimentTypes.Reference(another), _) => fieldTypeEncoder(RudimentTypes.Reference(another))

      case Field(RudimentTypes.Unknown, _) => "UNKNOWN"
      case _ => throw new IllegalArgumentException
    }) + (field match {
      case Field(_, FieldFlag.Optional) => "?"
      case Field(_, FieldFlag.Required) => "!"
      case Field(_, FieldFlag.WithDefault) => "+"
      case _ => ""
    })

    private def fieldTypeEncoder(t: FieldType): String = t match {
      case RudimentTypes.Text(_) => RudimentTypes.Text.toString
      case RudimentTypes.Number(_, _, _) => RudimentTypes.Number.toString

      case RudimentTypes.Date => RudimentTypes.Date.toString
      case RudimentTypes.Time => RudimentTypes.Time.toString
      case RudimentTypes.Timestamp => RudimentTypes.Timestamp.toString

      case RudimentTypes.Enum(n, v) => n.split("\\.").last + v.mkString("{", ",", "}")

      case s: RudimentTypes.Set => fieldEncoder(Field(s, FieldFlag.CanBeEmpty))
      case l: RudimentTypes.List => fieldEncoder(Field(l, FieldFlag.CanBeEmpty))
      case i: RudimentTypes.Index => fieldEncoder(Field(i, FieldFlag.CanBeEmpty))

      case RudimentTypes.Reference(another) => "*" + another.name

      case RudimentTypes.Unknown => "UNKNOWN"
      case _ => throw new IllegalArgumentException
    }

    override def apply(a: Field): Json = Encoder.encodeString.apply(fieldEncoder(a))
  }

  implicit def fieldTypeDecoder(implicit d: Decoder[Map[String, Any]]): Decoder[Field] = new Decoder[Field] {

    private def toFieldType(s: String): FieldType = s match {
      case "Text" => RudimentTypes.Text(Int.MaxValue)
      case "Number" => RudimentTypes.Number.ScalaInt

      case "Date" => RudimentTypes.Date
      case "Time" => RudimentTypes.Time
      case "Timestamp" => RudimentTypes.Timestamp
    }

    private def toFieldFlag(s: String): FieldFlag = FieldFlag.withName(s)

    private def toField(m: Map[String, Any]): Field = {
      if (m.contains("type") && m("type") == "Plain") {
        Field(toFieldType(m("kind").asInstanceOf[String]), toFieldFlag(m("flag").asInstanceOf[String]))
      } else if (m.contains("type") && m("type") == "Collection") {
        if (m.contains("set")) {
          Field(RudimentTypes.Set(toFieldType(m("set").asInstanceOf[String])), toFieldFlag(m("flag").asInstanceOf[String]))
        } else if (m.contains("list")) {
          Field(RudimentTypes.List(toFieldType(m("list").asInstanceOf[String])), toFieldFlag(m("list").asInstanceOf[String]))
        } else if (m.contains("index")) {
          Field(
            RudimentTypes.Index(
              toFieldType(m("index").asInstanceOf[String]),
              toFieldType(m("over").asInstanceOf[String])
            ),
            toFieldFlag(m("list").asInstanceOf[String])
          )
        } else {
          throw new IllegalArgumentException("Wrong structure")
        }
      } else {
        throw new IllegalArgumentException("Wrong structure")
      }
    }

    override def apply(c: HCursor): Result[Field] = Decoder[Map[String, Any]].map(toField)(c)
  }
}
