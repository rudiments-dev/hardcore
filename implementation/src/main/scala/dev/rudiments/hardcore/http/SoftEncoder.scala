package dev.rudiments.hardcore.http

import dev.rudiments.hardcore.types._
import enumeratum.EnumEntry
import io.circe.{Encoder, Json}

object SoftEncoder {
  def apply(implicit t: Type): Encoder[Instance] = new Encoder[Instance] {
    override def apply(instance: Instance): Json = instance match {
      case soft: SoftInstance => Json.obj(
        t.fields.map { case (name, field) =>
          name -> fieldEncoder(field)(soft.fields(name))
        }.toSeq :_*
      )
    }
  }

  private def fieldEncoder(field: Field): Any => Json = field match {
    case Field(f, FieldFlag.Required) => value => requiredFieldEncoder(f)(value)
    case Field(f, FieldFlag.WithDefault) => value => requiredFieldEncoder(f)(value)
    case Field(f, FieldFlag.Optional) => value => optionalFieldEncoder(f)(value)

    case Field(Types.List(of), _) => value => Json.arr(
      value
        .asInstanceOf[Iterable[_]]
        .map(v => requiredFieldEncoder(of)(v))
        .toSeq: _*
    )
    case Field(Types.Index(Types.Text(_), over), _) => value => Json.obj(
      value
        .asInstanceOf[Map[String, _]]
        .mapValues(v => requiredFieldEncoder(over)(v))
        .toSeq: _*
    )

    case other => ???
  }

  private def requiredFieldEncoder(f: FieldType): Any => Json = value => f match {
    case Types.Bool => Json.fromBoolean(value.asInstanceOf[Boolean])

    case Types.Text(_) => Json.fromString(value.asInstanceOf[String])

    case ScalaTypes.ScalaByte => Json.fromInt(value.asInstanceOf[Byte].toInt)
    case ScalaTypes.ScalaShort => Json.fromInt(value.asInstanceOf[Short].toInt)
    case ScalaTypes.ScalaInt => Json.fromInt(value.asInstanceOf[Int])
    case ScalaTypes.ScalaLong => Json.fromLong(value.asInstanceOf[Long])

    case ScalaTypes.ScalaFloat => Json.fromFloat(value.asInstanceOf[Float]).get
    case ScalaTypes.ScalaDouble => Json.fromDouble(value.asInstanceOf[Double]).get

    case ScalaTypes.ScalaBigInteger => Json.fromBigInt(value.asInstanceOf[BigInt])
    case ScalaTypes.ScalaBigDecimal => Json.fromBigDecimal(value.asInstanceOf[BigDecimal])
    case Types.Number(_, _, NumberFormat.Integer) => Json.fromBigInt(value.asInstanceOf[BigInt])
    case Types.Number(_, _, NumberFormat.Decimal) => Json.fromBigDecimal(value.asInstanceOf[BigDecimal])
    case Types.Number(_, _, NumberFormat.Float) => Json.fromBigDecimal(value.asInstanceOf[BigDecimal])

    case Types.Date => Json.fromString(value.asInstanceOf[java.sql.Date].toString)
    case Types.Time => Json.fromString(value.asInstanceOf[java.sql.Time].toString)
    case Types.Timestamp => Json.fromString(value.asInstanceOf[java.sql.Timestamp].toString)

    case Types.UUID => Json.fromString(value.asInstanceOf[java.util.UUID].toString)

    case Types.Reference(of) => referenceEncoder(of, value)

    case other => ???
  }

  private def referenceEncoder(of: Thing, value: Any): Json = of match {
    case s: Singleton => Json.fromString(s.name)
    case _: Declaration => ??? //TODO typeSystem.descendants(d)
    case t: Type =>SoftEncoder(t)(value.asInstanceOf[Instance])
    case _: Enum => value match {
      case v: EnumEntry => Json.fromString(v.entryName)
      case v: SoftEnum => Json.fromString(v.name)
    }
    case Algebraic(a, _, candidates) => value match {
      case s: Singleton => candidates
        .collectFirst { case cs: Singleton if cs == s => cs.name}
        .map(Json.fromString)
        .getOrElse(throw new NotSupportedSingleton(a, s))
      case i: SoftInstance => candidates
        .collectFirst { case ct: Type if ct == i.t => ct}
        .map { ct => SoftEncoder(ct)(i) }
        .getOrElse(throw new NotSupportedInstanceType(a, i))
      case other => ???
    }
  }

  class NotSupportedInstanceType(a: String, i: SoftInstance) extends RuntimeException(s"Instance type ${i.t} not supported in Algebraic $a")
  class NotSupportedSingleton(a: String, s: Singleton) extends RuntimeException(s"Singleton ${s.name} not supported in Algebraic $a")

  private def optionalFieldEncoder(f: FieldType): Any => Json = value => (f match {
    case Types.Bool => value.asInstanceOf[Option[Boolean]].map(Json.fromBoolean)

    case Types.Text(_) => value.asInstanceOf[Option[String]].map(Json.fromString)

    case ScalaTypes.ScalaByte => value.asInstanceOf[Option[Byte]].map(b => Json.fromInt(b.toInt))
    case ScalaTypes.ScalaShort => value.asInstanceOf[Option[Short]].map(b => Json.fromInt(b.toInt))
    case ScalaTypes.ScalaInt => value.asInstanceOf[Option[Int]].map(Json.fromInt)
    case ScalaTypes.ScalaLong => value.asInstanceOf[Option[Long]].map(Json.fromLong)

    case ScalaTypes.ScalaFloat => value.asInstanceOf[Option[Float]].flatMap(Json.fromFloat)
    case ScalaTypes.ScalaDouble => value.asInstanceOf[Option[Double]].flatMap(Json.fromDouble)

    case ScalaTypes.ScalaBigInteger => value.asInstanceOf[Option[BigInt]].map(Json.fromBigInt)
    case ScalaTypes.ScalaBigDecimal =>value.asInstanceOf[Option[BigDecimal]].map(Json.fromBigDecimal)
    case Types.Number(_, _, NumberFormat.Integer) => value.asInstanceOf[Option[BigInt]].map(Json.fromBigInt)
    case Types.Number(_, _, NumberFormat.Decimal) => value.asInstanceOf[Option[BigDecimal]].map(Json.fromBigDecimal)
    case Types.Number(_, _, NumberFormat.Float) => value.asInstanceOf[Option[BigDecimal]].map(Json.fromBigDecimal)

    case Types.Date => value.asInstanceOf[Option[java.sql.Date]].map(b => Json.fromString(b.toString))
    case Types.Time => value.asInstanceOf[Option[java.sql.Time]].map(b => Json.fromString(b.toString))
    case Types.Timestamp => value.asInstanceOf[Option[java.sql.Timestamp]].map(b => Json.fromString(b.toString))

    case Types.UUID => value.asInstanceOf[Option[java.util.UUID]].map(b => Json.fromString(b.toString))

    case Types.Reference(of) => value.asInstanceOf[Option[Any]].map(v => referenceEncoder(of, v))

    case other => ???
  }).getOrElse(Json.Null)
}
