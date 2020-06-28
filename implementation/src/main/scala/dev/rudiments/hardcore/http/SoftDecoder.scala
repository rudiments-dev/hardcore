package dev.rudiments.hardcore.http


import dev.rudiments.hardcore.types._
import io.circe.Decoder.Result
import io.circe.{Decoder, DecodingFailure, HCursor, KeyDecoder}

object SoftDecoder {

  def apply(implicit t: Type): Decoder[Instance] = new Decoder[Instance] {
    override def apply(c: HCursor): Result[Instance] = {
      t.fields.map {
        case (name, Field(Types.Reference(of), FieldFlag.Required))    => c.downField(name).as(referenceEncoder(of))
        case (name, Field(Types.Reference(of), FieldFlag.Optional))    => c.downField(name).as(Decoder.decodeOption(referenceEncoder(of)))
        case (name, Field(Types.Reference(of), FieldFlag.WithDefault)) => c.downField(name).as(referenceEncoder(of))

        case (name, Field(f, FieldFlag.Required))     => c.downField(name).as(plainRequiredFieldDecoder(f))
        case (name, Field(f, FieldFlag.Optional))     => c.downField(name).as(Decoder.decodeOption(plainRequiredFieldDecoder(f)))
        case (name, Field(f, FieldFlag.WithDefault))  => c.downField(name).as(Decoder.decodeOption(plainRequiredFieldDecoder(f)).map(_.get))//TODO add default values for common cases

        case (name, Field(Types.List(of), _)) => c.downField(name).as(Decoder.decodeSeq(plainRequiredFieldDecoder(of)))

        case (name, Field(Types.Index(Types.Text(_), over), _)) => c.downField(name).as(Decoder.decodeMap(KeyDecoder.decodeKeyString, plainRequiredFieldDecoder(over)))

      }.foldRight(Right(scala.Nil): Either[DecodingFailure, scala.List[Any]]) {
        (e, acc) => for (xs <- acc.right; x <- e.right) yield x :: xs
      }.map(values => SoftInstance(values: _*)(t))
    }
  }

  private def referenceEncoder(of: Thing): Decoder[_] = of match {
    case s: Singleton => Decoder.decodeString.map(str => if(s.name == str) s else ???)
    case _: Declaration => ??? //TODO typeSystem.descendants(d)
    case t: Type => SoftDecoder(t)
    case e: Enum => Decoder.decodeString.map(i => SoftEnum(e, e.candidates.map(_.toString).indexOf(i)))
    case a: Algebraic => a.candidates.map(referenceEncoder(_).asInstanceOf[Decoder[Any]]).reduce(_ or _)
  }

  private def plainRequiredFieldDecoder(f: FieldType): Decoder[_] = f match {
    case Types.Bool => Decoder.decodeBoolean

    case Types.Text(_) => Decoder.decodeString

    case ScalaTypes.ScalaByte => Decoder.decodeByte
    case ScalaTypes.ScalaShort => Decoder.decodeShort
    case ScalaTypes.ScalaInt => Decoder.decodeInt
    case ScalaTypes.ScalaLong => Decoder.decodeLong

    case ScalaTypes.ScalaFloat => Decoder.decodeFloat
    case ScalaTypes.ScalaDouble => Decoder.decodeDouble

    case ScalaTypes.ScalaBigInteger => Decoder.decodeBigInt
    case ScalaTypes.ScalaBigDecimal => Decoder.decodeBigDecimal
    case Types.Number(_, _, NumberFormat.Integer) => Decoder.decodeBigInt
    case Types.Number(_, _, NumberFormat.Decimal) => Decoder.decodeBigDecimal
    case Types.Number(_, _, NumberFormat.Float) => Decoder.decodeBigDecimal

    case Types.Date => Decoder.decodeString.map(java.sql.Date.valueOf)
    case Types.Time => Decoder.decodeString.map(java.sql.Time.valueOf)
    case Types.Timestamp => Decoder.decodeString.map(java.sql.Timestamp.valueOf)

    case Types.UUID => Decoder.decodeUUID

    case other => ???
  }
}
