package dev.rudiments.hardcore.http

import dev.rudiments.hardcore._
import io.circe.{Decoder, DecodingFailure, HCursor, KeyDecoder}

object ThingDecoder {
  val discriminator: String = "type"
  val id: ID = ID("decoders")
  val path: Path = id.asPath

  def init(implicit space: Space): Unit = {
    space(Create(id, new Memory(All, All)))
    path << Apply(Seq()) //TODO?
  }

  def decoder(thing: Thing): Decoder[_] =
    thing match {
      case p: Plain => plainDecoder(p)
      case List(of) => Decoder.decodeSeq(decoder(of))
      case Index(Plain.Text(_), over) => Decoder.decodeMap(KeyDecoder.decodeKeyString, decoder(over))
//      case a: Abstract => abstractDecoder(a)
      case ref: Ref => dataDecoder(ref)
      case t: Type => dataDecoder(t)
      case _ => ???
    }

  // Abstract = Data(type) || Ref (name, type?)
//  def abstractDecoder(a: Abstract): Decoder[_] = new Decoder[Any] {
//    override def apply(c: HCursor): Result[Any] = {
//      c.downField(discriminator).as[String].flatMap { name =>
//        domain.afterParent(a, name) match {
//          case spec: Spec if spec.fullName == "dev.rudiments.domain.Abstract" =>
//            typeDecoder(spec).apply(c).map(_.toScala[Abstract]) // ???
//          case spec: Spec => typeDecoder(spec).apply(c)
//          case one: The => Right(one)
//          case other => ???
//        }
//      }
//    }
//  }

  def dataDecoder(t: Type): Decoder[Data] =
    dataValuesDecoder(t).map(values => Data(t, values))

  def dataDecoder(ref: Ref): Decoder[Data] = ref.p match {
    case t: Type => dataValuesDecoder(t).map(values => Data(ref, values))
    case other => throw new IllegalArgumentException(s"Not supported: $other")
  }

  def dataValuesDecoder(t: Type): Decoder[scala.List[_]] = (c: HCursor) => t.fields.map {
    case Field(name, p, true) => c.downField(name).as(decoder(p))
    case Field(name, p, false) => c.downField(name).as(Decoder.decodeOption(decoder(p)))
  }.foldRight(Right(scala.Nil): Either[DecodingFailure, scala.List[_]]) {
    (e, acc) => for (xs <- acc.right; x <- e.right) yield x :: xs
  }

  private val plainDecoder: PartialFunction[Thing, Decoder[_]] = {
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
    case other => throw new IllegalArgumentException(s"Not supported: $other")
  }
}
