package dev.rudiments.types.registry.module

import dev.rudiments.hardcore.types._
import io.circe.Decoder.Result
import io.circe._

object FieldFormat {
  implicit val fieldEncoder: Encoder[Field] = new Encoder[Field] {
    private def fieldEncoder(field: Field): String = (field match {
      case Field(RudimentTypes.Number, _) => fieldTypeEncoder(RudimentTypes.Number)
      case Field(RudimentTypes.Text, _) => fieldTypeEncoder(RudimentTypes.Text)

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
      case RudimentTypes.Text => RudimentTypes.Text.toString
      case RudimentTypes.Number => RudimentTypes.Number.toString

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

  implicit val typeDecoder: Decoder[Type] = new Decoder[Type] {
    override def apply(c: HCursor): Result[Type] = implicitly[Decoder[String]].apply(c).flatMap { s =>
      ???
    }

    private def decodeFieldType(s: String): FieldType = {
            if(s.startsWith(RudimentTypes.Text.toString)) RudimentTypes.Text
      else  if(s.startsWith(RudimentTypes.Number.toString)) RudimentTypes.Number
      else  if(s.startsWith(RudimentTypes.Date.toString)) RudimentTypes.Date
      else  if(s.startsWith(RudimentTypes.Time.toString)) RudimentTypes.Time
      else  if(s.startsWith(RudimentTypes.Timestamp.toString)) RudimentTypes.Timestamp
              //TODO enum,
      else  RudimentTypes.Unknown
    }
  }
}
