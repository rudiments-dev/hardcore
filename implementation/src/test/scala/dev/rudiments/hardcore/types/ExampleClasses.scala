package dev.rudiments.hardcore.types

import java.sql.{Date, Time, Timestamp}
import java.util.UUID

import dev.rudiments.hardcore.data.MyEnum

case class Sample1 (
  a: Int,
  b: Option[String] = None,
  c: Set[String] = Set.empty
) extends DTO

case class Example(
  bool: Boolean,
  optBool: Option[Boolean],
  defaultBool: Boolean = true,

  string: String,
  optString: Option[String],
  defaultString: String = "default",
  defaultOptString: Option[String] = None,
  listOfStrings: Seq[String],

  byte: Byte,
  optByte: Option[Byte],
  short: Short,
  optShort: Option[Short],
  int: Int,
  optInt: Option[Int],
  long: Long,
  optLong: Option[Long],
  float: Float,
  optFloat: Option[Float],
  double: Double,
  optDouble: Option[Double],
  integer: BigInt,
  optInteger: Option[BigInt],
  decimal: BigDecimal,
  optDecimal: Option[BigDecimal],

  timestamp: Timestamp,
  optTimestamp: Option[Timestamp],
  date: Date,
  optDate: Option[Date],
  time: Time,
  optTime: Option[Time],

  uuid: UUID,
  optUuid: Option[UUID]
) extends DTO

case class Complicated1(
  a: Int,
  b: Option[Long],
  c: Set[String],
  d: Map[String, Int]
)

case class Complicated2(
  e: Sample1,
  f: Option[Sample1],
  g: Set[Sample1],
  h: Map[String, Sample1],
)

case class Complicated3(
  i: MyEnum,
  j: Option[MyEnum],
  k: Set[MyEnum],
  l: Map[String, MyEnum]
)