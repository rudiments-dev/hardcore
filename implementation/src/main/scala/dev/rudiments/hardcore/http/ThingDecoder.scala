package dev.rudiments.hardcore.http

import dev.rudiments.domain._
import io.circe.Decoder.Result
import io.circe.{Decoder, DecodingFailure, HCursor, KeyDecoder}

class ThingDecoder(domain: Domain, discriminator: String = "type") {

  def decoder(thing: Thing): Decoder[_] =
    thing match {
      case p: Plain => plainDecoder(p)
      case List(of) => Decoder.decodeSeq(decoder(of))
      case Index(Plain.Text(_), over) => Decoder.decodeMap(KeyDecoder.decodeKeyString, decoder(over))
      case a: Abstract => abstractDecoder(a)
      case spec: Spec => specDecoder(spec)
      case one: The => ??? //only for abstract cases - when name resolved via discriminator
      case other => ???
    }

  // Abstract = Instance(spec) || The (name)
  def abstractDecoder(a: Abstract): Decoder[_] = new Decoder[Any] {
    override def apply(c: HCursor): Result[Any] = {
      c.downField(discriminator).as[String].flatMap { name =>
        domain.afterParent(a, name) match {
          case spec: Spec if spec.fullName == "dev.rudiments.domain.Abstract" =>
            specDecoder(spec).apply(c).map(_.toScala[Abstract]) // ???
          case spec: Spec => specDecoder(spec).apply(c)
          case one: The => Right(one)
          case other => ???
        }
      }
    }
  }

  def abstractInstanceDecoder(name: String): Decoder[Instance] = new Decoder[Instance] {
    val a: Abstract = domain.find[Abstract](name)

    override def apply(c: HCursor): Result[Instance] = {
      c.downField(discriminator).as[String].flatMap { name =>
        domain.afterParent(a, name) match {
          case spec: Spec => specDecoder(spec).apply(c)
          case other => ???
        }
      }
    }
  }

  def specDecoder(name: String): Decoder[Instance] = specDecoder(domain.find[Spec](name))

  def specDecoder(spec: Spec): Decoder[Instance] = new Decoder[Instance] {
    override def apply(c: HCursor): Result[Instance] = {
      spec.fields.map {
        case (name, ValueSpec(t, true))  => c.downField(name).as(decoder(t))
        case (name, ValueSpec(t, false)) => c.downField(name).as(Decoder.decodeOption(decoder(t)))
      }.foldRight(Right(scala.Nil): Either[DecodingFailure, scala.List[Any]]) {
        (e, acc) => for (xs <- acc.right; x <- e.right) yield x :: xs
      }.map(values => Instance(spec, values))
    }
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
  }
}
