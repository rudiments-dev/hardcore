package dev.rudiments.hardcore.types

import scala.reflect.ClassTag

object ScalaTypes {
  object ScalaString extends Types.Text(MaxInt) {
    override def validate(arg: Any): Either[ValidationError, String] = ScalaStrictValid[String](arg)
  }

  object ScalaByte   extends Types.Number(MinByte,  MaxByte,   NumberFormat.Integer) {
    override def validate(arg: Any): Either[ValidationError, Byte] = ScalaStrictValid[Byte](arg)
  }
  object ScalaShort  extends Types.Number(MinShort, MaxShort,  NumberFormat.Integer) {
    override def validate(arg: Any): Either[ValidationError, Short] = ScalaStrictValid[Short](arg)
  }
  object ScalaInt    extends Types.Number(MinInt,   MaxInt,    NumberFormat.Integer) {
    override def validate(arg: Any): Either[ValidationError, Int] = ScalaStrictValid[Int](arg)
  }
  object ScalaLong   extends Types.Number(MinLong,  MaxLong,   NumberFormat.Integer) {
    override def validate(arg: Any): Either[ValidationError, Long] = ScalaStrictValid[Long](arg)
  }

  object ScalaFloat  extends Types.Number(MinFloat,   MaxFloat,  NumberFormat.Float) {
    override def validate(arg: Any): Either[ValidationError, Float] = ScalaStrictValid[Float](arg)
  }
  object ScalaDouble extends Types.Number(MinDouble,  MaxDouble, NumberFormat.Float) {
    override def validate(arg: Any): Either[ValidationError, Double] = ScalaStrictValid[Double](arg)
  }

  object ScalaBigInteger extends Types.Number(NegativeInfinity, PositiveInfinity, NumberFormat.Integer) {
    override def validate(arg: Any): Either[ValidationError, BigInt] = ScalaStrictValid[BigInt](arg)
  }
  object ScalaBigDecimal extends Types.Number(NegativeInfinity, PositiveInfinity, NumberFormat.Decimal) {
    override def validate(arg: Any): Either[ValidationError, BigDecimal] = ScalaStrictValid[BigDecimal](arg)
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

case class IncompatibleScalaType(typeName: String, arg: Any) extends ValidationError

object ScalaStrictValid {
  import scala.reflect.runtime.universe._
  def apply[T : TypeTag : ClassTag](arg: Any): Either[ValidationError, T] = {
    arg match {
      case t: T => Right(t)
      case _ => Left(IncompatibleScalaType(typeOf[T].typeSymbol.asClass.fullName, arg))
    }
  }
}