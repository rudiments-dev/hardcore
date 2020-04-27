package dev.rudiments.hardcore.types

object ScalaTypes {
  object ScalaString extends Types.Text(MaxInt)

  object ScalaByte   extends Types.Number(MinByte,  MaxByte,   NumberFormat.Integer)
  object ScalaShort  extends Types.Number(MinShort, MaxShort,  NumberFormat.Integer)
  object ScalaInt    extends Types.Number(MinInt,   MaxInt,    NumberFormat.Integer)
  object ScalaLong   extends Types.Number(MinLong,  MaxLong,   NumberFormat.Integer)

  object ScalaFloat  extends Types.Number(MinFloat,   MaxFloat,  NumberFormat.Float)
  object ScalaDouble extends Types.Number(MinDouble,  MaxDouble, NumberFormat.Float)

  object ScalaBigInteger extends Types.Number(NegativeInfinity, PositiveInfinity, NumberFormat.Integer)
  object ScalaBigDecimal extends Types.Number(NegativeInfinity, PositiveInfinity, NumberFormat.Decimal)


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
