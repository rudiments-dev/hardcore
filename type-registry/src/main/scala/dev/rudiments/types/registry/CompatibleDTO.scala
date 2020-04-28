package dev.rudiments.types.registry

import java.sql.{Date, Time, Timestamp}

import dev.rudiments.hardcore.types.{DTO, Defaults}
import enumeratum.{Enum, EnumEntry}

import scala.collection.immutable

sealed trait MyEnum extends EnumEntry
object MyEnum extends Enum[MyEnum] {
  override def values: immutable.IndexedSeq[MyEnum] = findValues

  case object One extends MyEnum
  case object Two extends MyEnum
  case object Red extends MyEnum
}

case class CompatibleDTO(
  id: Long,
  name: String,
  comment: Option[String] = None,
  n: Int = Int.MaxValue,
  array: Seq[Int],
  arrayWithDefault: Seq[Int] = Seq.empty,
  question: List[Int] = List(42),
  when: Timestamp = Defaults.now,
  date: Option[Date] = Some(Defaults.today),
  mapIntDate: Map[Int, Date] = Map.empty,
  sample: CompatiblePlainDTO,
  optSample: Option[CompatiblePlainDTO],
  seqSample: Seq[CompatiblePlainDTO] = Seq.empty,
  setSample: Set[CompatiblePlainDTO] = Set.empty,
  mapSample: Map[String, CompatiblePlainDTO] = Map.empty,
  multimapSample: Map[String, Set[CompatiblePlainDTO]] = Map.empty,
  deepMapSample: Map[String, Map[String, Set[CompatiblePlainDTO]]] = Map.empty,
) extends DTO

case class CompatiblePlainDTO(
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

  enum: MyEnum,
  optEnum: Option[MyEnum]
) extends DTO