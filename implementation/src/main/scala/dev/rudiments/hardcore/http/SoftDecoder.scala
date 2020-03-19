package dev.rudiments.hardcore.http


import dev.rudiments.hardcore.types.RudimentTypes._
import dev.rudiments.hardcore.types._
import io.circe.Decoder.Result
import io.circe.{Decoder, DecodingFailure, HCursor, KeyDecoder}

object SoftDecoder {

  import dev.rudiments.hardcore.types.RudimentTypes.Number._
  def apply(implicit t: Type): Decoder[SoftInstance] = new Decoder[SoftInstance] {
    override def apply(c: HCursor): Result[SoftInstance] = {
      t.fields.map {
        case (name, Field(Reference(t), FieldFlag.Required)) =>
          c.downField(name).as(SoftDecoder(t))
        case (name, Field(Reference(t), FieldFlag.Optional)) =>
          c.downField(name).as(Decoder.decodeOption(SoftDecoder(t)))
        case (name, Field(Reference(t), FieldFlag.WithDefault)) =>
          c.downField(name).as(SoftDecoder(t))

        case (name, Field(f, FieldFlag.Required)) =>
          c.downField(name).as(plainRequiredFieldDecoder(f))
        case (name, Field(f, FieldFlag.Optional)) =>
          c.downField(name).as(Decoder.decodeOption(plainRequiredFieldDecoder(f)))
        case (name, Field(f, FieldFlag.WithDefault)) =>
          c.downField(name).as(Decoder.decodeOption(plainRequiredFieldDecoder(f)).map(_.get))//TODO add default values for common cases

        case (name, Field(List(of), _)) =>
          c.downField(name).as(Decoder.decodeSeq(plainRequiredFieldDecoder(of)))
        case (name, Field(Index(Text(_), over), _)) =>
          c.downField(name).as(Decoder.decodeMap(KeyDecoder.decodeKeyString, plainRequiredFieldDecoder(over)))
      }.foldRight(Right(scala.Nil): Either[DecodingFailure, scala.List[Any]]) {
        (e, acc) => for (xs <- acc.right; x <- e.right) yield x :: xs
      }.map(values => SoftInstance(values)(t))
    }
  }

  private def plainRequiredFieldDecoder(f: FieldType): Decoder[_] = f match {
    case Bool => Decoder.decodeBoolean

    case Text(_) => Decoder.decodeString

    case ScalaByte => Decoder.decodeByte
    case ScalaShort => Decoder.decodeShort
    case ScalaInt => Decoder.decodeInt
    case ScalaLong => Decoder.decodeLong

    case ScalaFloat => Decoder.decodeFloat
    case ScalaDouble => Decoder.decodeDouble

    case ScalaBigInteger => Decoder.decodeBigInt
    case ScalaBigDecimal => Decoder.decodeBigDecimal
    case Number(_, _, NumberFormat.Integer) => Decoder.decodeBigInt
    case Number(_, _, NumberFormat.Decimal) => Decoder.decodeBigDecimal
    case Number(_, _, NumberFormat.Float) => Decoder.decodeBigDecimal

    case Date => Decoder.decodeString.map(java.sql.Date.valueOf)
    case Time => Decoder.decodeString.map(java.sql.Time.valueOf)
    case Timestamp => Decoder.decodeString.map(java.sql.Timestamp.valueOf)

    case e@Enum(_, values) => Decoder.decodeString.map(i => SoftEnum(e, values.indexOf(i)))
  }
}
