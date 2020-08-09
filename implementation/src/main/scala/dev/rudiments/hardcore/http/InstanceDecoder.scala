package dev.rudiments.hardcore.http


import dev.rudiments.types._
import dev.rudiments.types.hard.ScalaTypes
import io.circe.Decoder.Result
import io.circe.{Decoder, DecodingFailure, HCursor, KeyDecoder}

class InstanceDecoder(typeSystem: TypeSystem) {

  def apply(implicit t: Type): Decoder[Instance] = new Decoder[Instance] {
    override def apply(c: HCursor): Result[Instance] = {
      t.fields.values.map {
        case Field(name, f, true, _)    => c.downField(name).as(fieldDecoder(f))
        case Field(name, f, false, _)    => c.downField(name).as(Decoder.decodeOption(fieldDecoder(f)))
      }.foldRight(Right(scala.Nil): Either[DecodingFailure, scala.List[Any]]) {
        (e, acc) => for (xs <- acc.right; x <- e.right) yield x :: xs
      }.map(values => Instance(values: _*)(t))
    }
  }

  private def fieldDecoder(f: Thing): Decoder[_] = f match {
    case Plain.Bool => Decoder.decodeBoolean

    case Plain.Text(_) => Decoder.decodeString

    case ScalaTypes.ScalaByte => Decoder.decodeByte
    case ScalaTypes.ScalaShort => Decoder.decodeShort
    case ScalaTypes.ScalaInt => Decoder.decodeInt
    case ScalaTypes.ScalaLong => Decoder.decodeLong

    case ScalaTypes.ScalaFloat => Decoder.decodeFloat
    case ScalaTypes.ScalaDouble => Decoder.decodeDouble

    case ScalaTypes.ScalaBigInteger => Decoder.decodeBigInt
    case ScalaTypes.ScalaBigDecimal => Decoder.decodeBigDecimal
    case Plain.Number(_, _, NumberFormat.Integer) => Decoder.decodeBigInt
    case Plain.Number(_, _, NumberFormat.Decimal) => Decoder.decodeBigDecimal
    case Plain.Number(_, _, NumberFormat.Float) => Decoder.decodeBigDecimal

    case Plain.Date => Decoder.decodeString.map(java.sql.Date.valueOf)
    case Plain.Time => Decoder.decodeString.map(java.sql.Time.valueOf)
    case Plain.Timestamp => Decoder.decodeString.map(java.sql.Timestamp.valueOf)

    case Plain.UUID => Decoder.decodeUUID

    case List(of) => Decoder.decodeSeq(fieldDecoder(of))
    case Index(Plain.Text(_), over) => Decoder.decodeMap(KeyDecoder.decodeKeyString, fieldDecoder(over))

    case a: Abstract =>
      val descendants = typeSystem.descendants(a)
      descendants.map(fieldDecoder(_).asInstanceOf[Decoder[Any]]).reduce(_ or _)
    case o: OnlyOne => Decoder.decodeString.map(str => if(o.name == str) o else ???)
    case t: Type => this.apply(t)

    case other => ???
  }
}
