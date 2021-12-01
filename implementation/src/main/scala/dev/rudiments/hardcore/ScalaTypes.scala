package dev.rudiments.hardcore

import java.sql.{Date, Time, Timestamp}
import java.util.UUID



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

    case (p, v) => throw new IllegalArgumentException(s"Incompatible $p with value $v")
  }

  import scala.reflect.runtime.universe.{Type => SysType, _}
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

  object ScalaString extends Plain.Text(MaxInt)
  object ScalaByte   extends Plain.Number(MinByte,  MaxByte,   NumberFormat.Integer)
  object ScalaShort  extends Plain.Number(MinShort, MaxShort,  NumberFormat.Integer)
  object ScalaInt    extends Plain.Number(MinInt,   MaxInt,    NumberFormat.Integer)
  object ScalaLong   extends Plain.Number(MinLong,  MaxLong,   NumberFormat.Integer)

  object ScalaFloat  extends Plain.Number(MinFloat,   MaxFloat,  NumberFormat.Float)
  object ScalaDouble extends Plain.Number(MinDouble,  MaxDouble, NumberFormat.Float)

  object ScalaBigInteger extends Plain.Number(Size.NegativeInfinity, Size.PositiveInfinity, NumberFormat.Integer)
  object ScalaBigDecimal extends Plain.Number(Size.NegativeInfinity, Size.PositiveInfinity, NumberFormat.Decimal)


  import dev.rudiments.hardcore.Size.Big
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
