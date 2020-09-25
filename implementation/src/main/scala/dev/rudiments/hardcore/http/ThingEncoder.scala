package dev.rudiments.hardcore.http

import java.sql.{Date, Time, Timestamp}

import dev.rudiments.domain._
import enumeratum.EnumEntry
import io.circe.{Encoder, Json, KeyEncoder}

class ThingEncoder(domain: Domain, discriminator: String = "type") {

  def encoder(thing: Thing): Encoder[_] = thing match {
    case p: Plain => plainEncoder(p)
    case List(of) => Encoder.encodeSeq(encoder(of))
    case Index(Plain.Text(_), over) => Encoder.encodeMap(KeyEncoder.encodeKeyString, encoder(over))
    case a: Abstract => abstractEncoder(a)
    case spec: Spec => specEncoder(spec)
    case one: The => ??? //only for abstract cases - when name resolved via discriminator
    case other => ???
  }

  def abstractEncoder(a: Abstract): Encoder[_] = new Encoder[Any] {
    override def apply(value: Any): Json = value match {
      case i: Instance => specEncoder(i.spec, true)(i)
      case The(name) => Json.obj(
        discriminator -> Encoder.encodeString(name)
      )
      case e: EnumEntry => Encoder.encodeString(domain.afterParent(a, e.entryName).name)
      case other => ???
    }
  }

  def abstractInstanceEncoder(name: String): Encoder[Instance] = new Encoder[Instance] {
    val a: Abstract = domain.find[Abstract](name)

    override def apply(value: Instance): Json = value match {
      case i: Instance =>
        domain.afterParent(a, i.spec.name)
        specEncoder(i.spec, true)(i)
      case other => ???
    }
  }

  def specEncoder(name: String): Encoder[Instance] = specEncoder(domain.find[Spec](name))

  def specEncoder(spec: Spec, withDiscriminator: Boolean = false): Encoder[Instance] = new Encoder[Instance] {
    override def apply(a: Instance): Json = {
      val fields = spec.fields.zip(a.values).map {
        case ((n, s), v) if s.isRequired => n -> encoder(s.thing).asInstanceOf[Encoder[Any]](v)
        case ((n, s), Some(v)) if !s.isRequired => n -> encoder(s.thing).asInstanceOf[Encoder[Any]](v)
        case ((n, s), None) if !s.isRequired => n -> Json.Null
      }.toSeq

      if(withDiscriminator) {
        val content = Seq(discriminator -> Encoder.encodeString(a.spec.name)) ++ fields
        Json.obj(content: _*)
      } else {
        Json.obj(fields: _*)
      }
    }
  }

  val plainEncoder: PartialFunction[Thing, Encoder[_]] = {
    case Plain.Bool => Encoder.encodeBoolean

    case Plain.Text(_) => Encoder.encodeString

    case ScalaTypes.ScalaByte => Encoder.encodeByte
    case ScalaTypes.ScalaShort => Encoder.encodeShort
    case ScalaTypes.ScalaInt => Encoder.encodeInt
    case ScalaTypes.ScalaLong => Encoder.encodeLong

    case ScalaTypes.ScalaFloat => Encoder.encodeFloat
    case ScalaTypes.ScalaDouble => Encoder.encodeDouble

    case ScalaTypes.ScalaBigInteger => Encoder.encodeBigInt
    case ScalaTypes.ScalaBigDecimal => Encoder.encodeBigDecimal
    case Plain.Number(_, _, NumberFormat.Integer) => Encoder.encodeBigInt
    case Plain.Number(_, _, NumberFormat.Decimal) => Encoder.encodeBigDecimal
    case Plain.Number(_, _, NumberFormat.Float) => Encoder.encodeBigDecimal

    case Plain.Date => Encoder.encodeString.contramap[Date](_.toString)
    case Plain.Time => Encoder.encodeString.contramap[Time](_.toString)
    case Plain.Timestamp => Encoder.encodeString.contramap[Timestamp](_.toString)

    case Plain.UUID => Encoder.encodeUUID
  }
}
