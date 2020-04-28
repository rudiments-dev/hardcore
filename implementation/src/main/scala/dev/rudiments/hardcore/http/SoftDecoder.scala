package dev.rudiments.hardcore.http


import dev.rudiments.hardcore.types._
import io.circe.Decoder.Result
import io.circe.{Decoder, DecodingFailure, HCursor, KeyDecoder}

object SoftDecoder {

  import dev.rudiments.hardcore.types.Types.Number._
  def apply(implicit t: Type): Decoder[SoftInstance] = new Decoder[SoftInstance] {
    override def apply(c: HCursor): Result[SoftInstance] = {
      t.fields.map {
        case (name, Field(Types.Reference(t), FieldFlag.Required)) =>
          c.downField(name).as(SoftDecoder(t))
        case (name, Field(Types.Reference(t), FieldFlag.Optional)) =>
          c.downField(name).as(Decoder.decodeOption(SoftDecoder(t)))
        case (name, Field(Types.Reference(t), FieldFlag.WithDefault)) =>
          c.downField(name).as(SoftDecoder(t))

        case (name, Field(f, FieldFlag.Required)) =>
          c.downField(name).as(plainRequiredFieldDecoder(f))
        case (name, Field(f, FieldFlag.Optional)) =>
          c.downField(name).as(Decoder.decodeOption(plainRequiredFieldDecoder(f)))
        case (name, Field(f, FieldFlag.WithDefault)) =>
          c.downField(name).as(Decoder.decodeOption(plainRequiredFieldDecoder(f)).map(_.get))//TODO add default values for common cases

        case (name, Field(Types.List(of), _)) =>
          c.downField(name).as(Decoder.decodeSeq(plainRequiredFieldDecoder(of)))
        case (name, Field(Types.Index(Types.Text(_), over), _)) =>
          c.downField(name).as(Decoder.decodeMap(KeyDecoder.decodeKeyString, plainRequiredFieldDecoder(over)))
      }.foldRight(Right(scala.Nil): Either[DecodingFailure, scala.List[Any]]) {
        (e, acc) => for (xs <- acc.right; x <- e.right) yield x :: xs
      }.map(values => SoftInstance(values: _*)(t))
    }
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

    case e@Types.Enum(_, values) => Decoder.decodeString.map(i => SoftEnum(e, values.indexOf(i)))
  }
}
