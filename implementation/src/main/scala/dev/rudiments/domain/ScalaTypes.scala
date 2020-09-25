package dev.rudiments.domain

import java.sql.{Date, Time, Timestamp}
import java.util.UUID

import dev.rudiments.domain.Size.Big

import scala.reflect.ClassTag

object ScalaTypes {
  def wrapPlain(plain: Plain, value: Any): Any = (plain, value) match {
    case (Plain.Bool,         i: Boolean) => i
    case (Plain.Text(_),      i: String) => i
    case (Plain.UUID,         i: UUID) => i
    case (ScalaByte,          i: Byte) => i
    case (ScalaShort,         i: Short) => i
    case (ScalaInt,           i: Int) => i
    case (ScalaLong,          i: Long) => i
    case (ScalaFloat,         i: Float) => i
    case (ScalaDouble,        i: Double) => i
    case (ScalaBigInteger,    i: BigInt) => i
    case (ScalaBigDecimal,    i: BigDecimal) => i
    case (Plain.Date,         i: java.sql.Date) => i
    case (Plain.Time,         i: java.sql.Time) => i
    case (Plain.Timestamp,    i: java.sql.Timestamp) => i

    case (p, v) => throw new IllegalArgumentException(s"Incompatible ${p.name} with value $v")
  }

  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.universe.{Type => SysType}
  val plain: Map[SysType, Plain] = Map(
    typeOf[Boolean] ->    Plain.Bool,
    typeOf[String] ->     ScalaTypes.ScalaString,
    typeOf[UUID] ->       Plain.UUID,

    typeOf[Byte] ->       ScalaTypes.ScalaByte,
    typeOf[Short] ->      ScalaTypes.ScalaShort,
    typeOf[Int] ->        ScalaTypes.ScalaInt,
    typeOf[Long] ->       ScalaTypes.ScalaLong,
    typeOf[Float] ->      ScalaTypes.ScalaFloat,
    typeOf[Double] ->     ScalaTypes.ScalaDouble,

    typeOf[BigInt] ->     ScalaTypes.ScalaBigInteger,
    typeOf[BigDecimal] -> ScalaTypes.ScalaBigDecimal,

    typeOf[Date] ->       Plain.Date,
    typeOf[Time] ->       Plain.Time,
    typeOf[Timestamp] ->  Plain.Timestamp
  )

  object ScalaString extends Plain.Text(MaxInt) {
    override def validate(system: Domain, value: Any): String = ScalaStrictValid[String](system, value)
  }

  object ScalaByte   extends Plain.Number(MinByte,  MaxByte,   NumberFormat.Integer) {
    override def validate(system: Domain, value: Any): Byte = ScalaStrictValid[Byte](system, value)
  }
  object ScalaShort  extends Plain.Number(MinShort, MaxShort,  NumberFormat.Integer) {
    override def validate(system: Domain, value: Any): Short = ScalaStrictValid[Short](system, value)
  }
  object ScalaInt    extends Plain.Number(MinInt,   MaxInt,    NumberFormat.Integer) {
    override def validate(system: Domain, value: Any): Int = ScalaStrictValid[Int](system, value)
  }
  object ScalaLong   extends Plain.Number(MinLong,  MaxLong,   NumberFormat.Integer) {
    override def validate(system: Domain, value: Any): Long = ScalaStrictValid[Long](system, value)
  }

  object ScalaFloat  extends Plain.Number(MinFloat,   MaxFloat,  NumberFormat.Float) {
    override def validate(system: Domain, value: Any): Float = ScalaStrictValid[Float](system, value)
  }
  object ScalaDouble extends Plain.Number(MinDouble,  MaxDouble, NumberFormat.Float) {
    override def validate(system: Domain, value: Any): Double = ScalaStrictValid[Double](system, value)
  }

  object ScalaBigInteger extends Plain.Number(Size.NegativeInfinity, Size.PositiveInfinity, NumberFormat.Integer) {
    override def validate(system: Domain, value: Any): BigInt = ScalaStrictValid[BigInt](system, value)
  }
  object ScalaBigDecimal extends Plain.Number(Size.NegativeInfinity, Size.PositiveInfinity, NumberFormat.Decimal) {
    override def validate(system: Domain, value: Any): BigDecimal = ScalaStrictValid[BigDecimal](system, value)
  }


  object MinByte extends Big(BigDecimal(Byte.MinValue))
  object MaxByte extends Big(BigDecimal(Byte.MaxValue))

  object MinShort extends Big(BigDecimal(Short.MinValue))
  object MaxShort extends Big(BigDecimal(Short.MaxValue))

  object MinInt extends Big(BigDecimal(Int.MinValue))
  object MaxInt extends Big(BigDecimal(Int.MaxValue))

  object MinLong extends Big(BigDecimal(Long.MinValue))
  object MaxLong extends Big(BigDecimal(Long.MaxValue))

  object MinFloat extends Big(BigDecimal.decimal(Float.MinValue))
  object MaxFloat extends Big(BigDecimal.decimal(Float.MaxValue))

  object MinDouble extends Big(BigDecimal.decimal(Double.MinValue))
  object MaxDouble extends Big(BigDecimal.decimal(Double.MaxValue))
}

object ScalaStrictValid {
  import scala.reflect.runtime.universe._
  def apply[T : TypeTag : ClassTag](system: Domain, value: Any): T = {
    value match {
      case t: T => t
      case _ => throw new IllegalArgumentException(s"Incompatible ${typeOf[T].typeSymbol.asClass.fullName} strict scala value $value")
    }
  }
}