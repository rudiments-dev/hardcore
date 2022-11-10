package dev.rudiments.hardcore.http

import dev.rudiments.hardcore._

object ScalaTypes {
  val numbers: Map[ID, Predicate] = Map(
    ID("Byte") -> Number(Byte.MinValue, Byte.MaxValue),
    ID("Short") -> Number(Short.MinValue, Short.MaxValue),
    ID("Char") -> Number(Char.MinValue, Char.MaxValue),
    ID("Int") -> Number(Int.MinValue, Int.MaxValue),
    ID("Long") -> Number(Long.MinValue, Long.MaxValue),
    ID("Float") -> Number(Float.MinValue, Float.MaxValue),
    ID("Double") -> Number(Double.MinValue, Double.MaxValue),
    ID("BigInteger") -> Number(Anything, Double.MaxValue),
  )

  val text: Map[ID, Predicate] = Map(
    ID("String") -> Text(Int.MaxValue),
    ID("DefaultText") -> Text(1024)
  )
}
