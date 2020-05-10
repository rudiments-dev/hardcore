package dev.rudiments.hardcore.http

import dev.rudiments.hardcore.types.{Field, FieldFlag, FieldType, NumberFormat, ScalaTypes, SoftInstance, Type, Types}
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

  private def requiredFieldEncoder(f: FieldType): Any => Json = f match {
    case Types.Bool => value => Json.fromBoolean(value.asInstanceOf[Boolean])

    case Types.Text(_) => value => Json.fromString(value.asInstanceOf[String])

    case ScalaTypes.ScalaByte => value => Json.fromInt(value.asInstanceOf[Byte].toInt)
    case ScalaTypes.ScalaShort => value => Json.fromInt(value.asInstanceOf[Short].toInt)
    case ScalaTypes.ScalaInt => value => Json.fromInt(value.asInstanceOf[Int])
    case ScalaTypes.ScalaLong => value => Json.fromLong(value.asInstanceOf[Long])

    case ScalaTypes.ScalaFloat => value => Json.fromFloat(value.asInstanceOf[Float]).get
    case ScalaTypes.ScalaDouble => value => Json.fromDouble(value.asInstanceOf[Double]).get

    case ScalaTypes.ScalaBigInteger => value => Json.fromBigInt(value.asInstanceOf[BigInt])
    case ScalaTypes.ScalaBigDecimal => value => Json.fromBigDecimal(value.asInstanceOf[BigDecimal])
    case Types.Number(_, _, NumberFormat.Integer) => value => Json.fromBigInt(value.asInstanceOf[BigInt])
    case Types.Number(_, _, NumberFormat.Decimal) => value => Json.fromBigDecimal(value.asInstanceOf[BigDecimal])
    case Types.Number(_, _, NumberFormat.Float) => value => Json.fromBigDecimal(value.asInstanceOf[BigDecimal])

    case Types.Date => value => Json.fromString(value.asInstanceOf[java.sql.Date].toString)
    case Types.Time => value => Json.fromString(value.asInstanceOf[java.sql.Time].toString)
    case Types.Timestamp => value => Json.fromString(value.asInstanceOf[java.sql.Timestamp].toString)

    case Types.UUID => value => Json.fromString(value.asInstanceOf[java.util.UUID].toString)

    case Types.Enum(_, _) => value => Json.fromString(value.asInstanceOf[EnumEntry].entryName)

    case Types.Reference(of) => value => SoftEncoder(of)(value.asInstanceOf[SoftInstance])

    case other => ???
  }

  private def optionalFieldEncoder(f: FieldType): Any => Json = f match {
    case Types.Bool => value => value.asInstanceOf[Option[Boolean]].map(Json.fromBoolean).getOrElse(Json.Null)

    case Types.Text(_) => value => value.asInstanceOf[Option[String]].map(Json.fromString).getOrElse(Json.Null)

    case ScalaTypes.ScalaByte => value => value.asInstanceOf[Option[Byte]].map(b => Json.fromInt(b.toInt)).getOrElse(Json.Null)
    case ScalaTypes.ScalaShort => value => value.asInstanceOf[Option[Short]].map(b => Json.fromInt(b.toInt)).getOrElse(Json.Null)
    case ScalaTypes.ScalaInt => value => value.asInstanceOf[Option[Int]].map(Json.fromInt).getOrElse(Json.Null)
    case ScalaTypes.ScalaLong => value => value.asInstanceOf[Option[Long]].map(Json.fromLong).getOrElse(Json.Null)

    case ScalaTypes.ScalaFloat => value => value.asInstanceOf[Option[Float]].map(Json.fromFloat).get.getOrElse(Json.Null)
    case ScalaTypes.ScalaDouble => value => value.asInstanceOf[Option[Double]].map(Json.fromDouble).get.getOrElse(Json.Null)

    case ScalaTypes.ScalaBigInteger => value => value.asInstanceOf[Option[BigInt]].map(Json.fromBigInt).getOrElse(Json.Null)
    case ScalaTypes.ScalaBigDecimal => value => value.asInstanceOf[Option[BigDecimal]].map(Json.fromBigDecimal).getOrElse(Json.Null)
    case Types.Number(_, _, NumberFormat.Integer) => value => value.asInstanceOf[Option[BigInt]].map(Json.fromBigInt).getOrElse(Json.Null)
    case Types.Number(_, _, NumberFormat.Decimal) => value => value.asInstanceOf[Option[BigDecimal]].map(Json.fromBigDecimal).getOrElse(Json.Null)
    case Types.Number(_, _, NumberFormat.Float) => value => value.asInstanceOf[Option[BigDecimal]].map(Json.fromBigDecimal).getOrElse(Json.Null)

    case Types.Date => value => value.asInstanceOf[Option[java.sql.Date]].map(b => Json.fromString(b.toString)).getOrElse(Json.Null)
    case Types.Time => value => value.asInstanceOf[Option[java.sql.Time]].map(b => Json.fromString(b.toString)).getOrElse(Json.Null)
    case Types.Timestamp => value => value.asInstanceOf[Option[java.sql.Timestamp]].map(b => Json.fromString(b.toString)).getOrElse(Json.Null)

    case Types.Enum(_, _) => value => value.asInstanceOf[Option[EnumEntry]].map(b => Json.fromString(b.entryName)).getOrElse(Json.Null)

    case Types.Reference(of) => value => value
      .asInstanceOf[Option[SoftInstance]]
      .map(v => SoftEncoder(of)(v))
      .getOrElse(Json.Null)

    case other => ???
  }
}
