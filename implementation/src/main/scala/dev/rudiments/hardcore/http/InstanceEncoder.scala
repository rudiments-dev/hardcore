package dev.rudiments.hardcore.http

import dev.rudiments.types._
import dev.rudiments.types.hard.ScalaTypes
import enumeratum.EnumEntry
import io.circe.{Encoder, Json}

class InstanceEncoder(typeSystem: TypeSystem) {

  def apply(implicit t: Type): Encoder[Instance] = new Encoder[Instance] {
    override def apply(a: Instance): Json = Json.obj(
      t.fields.map { case (name, field) =>
        name -> fieldEncoder(field)(a.fields(name))
      }.toSeq :_*
    )
  }

  private def fieldEncoder(field: Field): Any => Json = field match {
    case Field(_, f, true, _) =>
      value => requiredEncoder(f)(value)
    case Field(_, f, false, _) =>
      value => value.asInstanceOf[Option[_]].map(requiredEncoder(f)).getOrElse(Json.Null)

    case other => ???
  }

  private def requiredEncoder(f: Thing): Any => Json = value => f match {
    case Plain.Bool => Json.fromBoolean(value.asInstanceOf[Boolean])

    case Plain.Text(_) => Json.fromString(value.asInstanceOf[String])

    case ScalaTypes.ScalaByte => Json.fromInt(value.asInstanceOf[Byte].toInt)
    case ScalaTypes.ScalaShort => Json.fromInt(value.asInstanceOf[Short].toInt)
    case ScalaTypes.ScalaInt => Json.fromInt(value.asInstanceOf[Int])
    case ScalaTypes.ScalaLong => Json.fromLong(value.asInstanceOf[Long])

    case ScalaTypes.ScalaFloat => Json.fromFloat(value.asInstanceOf[Float]).get
    case ScalaTypes.ScalaDouble => Json.fromDouble(value.asInstanceOf[Double]).get

    case ScalaTypes.ScalaBigInteger => Json.fromBigInt(value.asInstanceOf[BigInt])
    case ScalaTypes.ScalaBigDecimal => Json.fromBigDecimal(value.asInstanceOf[BigDecimal])
    case Plain.Number(_, _, NumberFormat.Integer) => Json.fromBigInt(value.asInstanceOf[BigInt])
    case Plain.Number(_, _, NumberFormat.Decimal) => Json.fromBigDecimal(value.asInstanceOf[BigDecimal])
    case Plain.Number(_, _, NumberFormat.Float) => Json.fromBigDecimal(value.asInstanceOf[BigDecimal])

    case Plain.Date => Json.fromString(value.asInstanceOf[java.sql.Date].toString)
    case Plain.Time => Json.fromString(value.asInstanceOf[java.sql.Time].toString)
    case Plain.Timestamp => Json.fromString(value.asInstanceOf[java.sql.Timestamp].toString)

    case Plain.UUID => Json.fromString(value.asInstanceOf[java.util.UUID].toString)

    case List(of) => Json.arr(
      value
        .asInstanceOf[Iterable[_]]
        .map(v => requiredEncoder(of)(v))
        .toSeq: _*
    )
    case Index(Plain.Text(_), over) => Json.obj(
      value
        .asInstanceOf[Map[String, _]]
        .mapValues(v => requiredEncoder(over)(v))
        .toSeq: _*
    )

    case a: Abstract =>
      val descendans = typeSystem.descendants(a)
      value match {
        case v: EnumEntry => Json.fromString(v.entryName)
        case o: OnlyOne =>
          descendans
            .collectFirst { case cs: OnlyOne if cs == o => cs.name}
            .map(Json.fromString)
            .getOrElse(throw new NotSupportedSingleton(a.name, o))
        case i: Instance =>
          descendans
            .collectFirst { case ct: Type if ct == i.t => ct}
            .map( ct => this.apply(ct)(i))
            .getOrElse(throw new NotSupportedInstanceType(a.name, i))
      }
    case s: OnlyOne => Json.fromString(s.name)
    case t: Type => this.apply(t)(value.asInstanceOf[Instance])

    case other => ???
  }

  class NotSupportedInstanceType(a: String, i: Instance) extends RuntimeException(s"Instance type ${i.t} not supported in Algebraic $a")
  class NotSupportedSingleton(a: String, s: OnlyOne) extends RuntimeException(s"Singleton ${s.name} not supported in Algebraic $a")
}
