package dev.rudiments.hardcore.http

import dev.rudiments.hardcore.types.RudimentTypes._
import dev.rudiments.hardcore.types.{Field, FieldFlag, FieldType, NumberFormat, SoftInstance, Type}
import enumeratum.EnumEntry
import io.circe.{Encoder, Json}

object SoftEncoder {
  def apply(implicit t: Type): Encoder[SoftInstance] = new Encoder[SoftInstance] {
    override def apply(instance: SoftInstance): Json = Json.obj(
      t.fields.map { case (name, field) =>
        name -> fieldEncoder(field)(instance.fields(name))
      }.toSeq :_*
    )
  }

  import Number._
  private def fieldEncoder(field: Field): Any => Json = field match {
    case Field(f, FieldFlag.Required) => value => requiredFieldEncoder(f)(value)
    case Field(f, FieldFlag.WithDefault) => value => requiredFieldEncoder(f)(value)
    case Field(f, FieldFlag.Optional) => value => optionalFieldEncoder(f)(value)

    case Field(List(of), _) => value => Json.arr(
      value
        .asInstanceOf[Iterable[_]]
        .map(v => requiredFieldEncoder(of)(v))
        .toSeq: _*
    )
    case Field(Index(Text(_), over), _) => value => Json.obj(
      value
        .asInstanceOf[Map[String, _]]
        .mapValues(v => requiredFieldEncoder(over)(v))
        .toSeq: _*
    )
  }

  private def requiredFieldEncoder(f: FieldType): Any => Json = f match {
    case Bool => value => Json.fromBoolean(value.asInstanceOf[Boolean])

    case Text(_) => value => Json.fromString(value.asInstanceOf[String])

    case ScalaByte => value => Json.fromInt(value.asInstanceOf[Byte].toInt)
    case ScalaShort => value => Json.fromInt(value.asInstanceOf[Short].toInt)
    case ScalaInt => value => Json.fromInt(value.asInstanceOf[Int])
    case ScalaLong => value => Json.fromLong(value.asInstanceOf[Long])

    case ScalaFloat => value => Json.fromFloat(value.asInstanceOf[Float]).get
    case ScalaDouble => value => Json.fromDouble(value.asInstanceOf[Double]).get

    case ScalaBigInteger => value => Json.fromBigInt(value.asInstanceOf[BigInt])
    case ScalaBigDecimal => value => Json.fromBigDecimal(value.asInstanceOf[BigDecimal])
    case Number(_, _, NumberFormat.Integer) => value => Json.fromBigInt(value.asInstanceOf[BigInt])
    case Number(_, _, NumberFormat.Decimal) => value => Json.fromBigDecimal(value.asInstanceOf[BigDecimal])
    case Number(_, _, NumberFormat.Float) => value => Json.fromBigDecimal(value.asInstanceOf[BigDecimal])

    case Date => value => Json.fromString(value.asInstanceOf[java.sql.Date].toString)
    case Time => value => Json.fromString(value.asInstanceOf[java.sql.Time].toString)
    case Timestamp => value => Json.fromString(value.asInstanceOf[java.sql.Timestamp].toString)

    case Enum(_, _) => value => Json.fromString(value.asInstanceOf[EnumEntry].entryName)

    case Reference(of) => value => SoftEncoder(of)(value.asInstanceOf[SoftInstance])
  }

  private def optionalFieldEncoder(f: FieldType): Any => Json = f match {
    case Bool => value => value.asInstanceOf[Option[Boolean]].map(Json.fromBoolean).getOrElse(Json.Null)

    case Text(_) => value => value.asInstanceOf[Option[String]].map(Json.fromString).getOrElse(Json.Null)

    case ScalaByte => value => value.asInstanceOf[Option[Byte]].map(b => Json.fromInt(b.toInt)).getOrElse(Json.Null)
    case ScalaShort => value => value.asInstanceOf[Option[Short]].map(b => Json.fromInt(b.toInt)).getOrElse(Json.Null)
    case ScalaInt => value => value.asInstanceOf[Option[Int]].map(Json.fromInt).getOrElse(Json.Null)
    case ScalaLong => value => value.asInstanceOf[Option[Long]].map(Json.fromLong).getOrElse(Json.Null)

    case ScalaFloat => value => value.asInstanceOf[Option[Float]].map(Json.fromFloat).get.getOrElse(Json.Null)
    case ScalaDouble => value => value.asInstanceOf[Option[Double]].map(Json.fromDouble).get.getOrElse(Json.Null)

    case ScalaBigInteger => value => value.asInstanceOf[Option[BigInt]].map(Json.fromBigInt).getOrElse(Json.Null)
    case ScalaBigDecimal => value => value.asInstanceOf[Option[BigDecimal]].map(Json.fromBigDecimal).getOrElse(Json.Null)
    case Number(_, _, NumberFormat.Integer) => value => value.asInstanceOf[Option[BigInt]].map(Json.fromBigInt).getOrElse(Json.Null)
    case Number(_, _, NumberFormat.Decimal) => value => value.asInstanceOf[Option[BigDecimal]].map(Json.fromBigDecimal).getOrElse(Json.Null)
    case Number(_, _, NumberFormat.Float) => value => value.asInstanceOf[Option[BigDecimal]].map(Json.fromBigDecimal).getOrElse(Json.Null)

    case Date => value => value.asInstanceOf[Option[java.sql.Date]].map(b => Json.fromString(b.toString)).getOrElse(Json.Null)
    case Time => value => value.asInstanceOf[Option[java.sql.Time]].map(b => Json.fromString(b.toString)).getOrElse(Json.Null)
    case Timestamp => value => value.asInstanceOf[Option[java.sql.Timestamp]].map(b => Json.fromString(b.toString)).getOrElse(Json.Null)

    case Enum(_, _) => value => value.asInstanceOf[Option[EnumEntry]].map(b => Json.fromString(b.entryName)).getOrElse(Json.Null)

    case Reference(of) => value => value
      .asInstanceOf[Option[SoftInstance]]
      .map(v => SoftEncoder(of)(v))
      .getOrElse(Json.Null)
  }
}
