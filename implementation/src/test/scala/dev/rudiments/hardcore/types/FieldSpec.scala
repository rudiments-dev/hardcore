package dev.rudiments.hardcore.types

import java.sql.{Date, Time, Timestamp}
import java.util.UUID

import org.junit.runner.RunWith
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FieldSpec extends WordSpec with Matchers {
  val t: HardType[Example] = HardType[Example]

  "Type transforms all basic types to BasicTypes" in {
    t.fields should be (Map(
      "bool" ->         Field(Types.Bool, FieldFlag.Required),
      "optBool" ->      Field(Types.Bool, FieldFlag.Optional),
      "defaultBool" ->  Field(Types.Bool, FieldFlag.WithDefault),

      "string" ->           Field(ScalaTypes.ScalaString, FieldFlag.Required),
      "optString" ->        Field(ScalaTypes.ScalaString, FieldFlag.Optional),
      "defaultString" ->    Field(ScalaTypes.ScalaString, FieldFlag.WithDefault),
      "defaultOptString" -> Field(ScalaTypes.ScalaString, FieldFlag.Optional),
      "listOfStrings" ->    Field(Types.List(ScalaTypes.ScalaString), FieldFlag.NonEmpty),

      "byte" ->       Field(ScalaTypes.ScalaByte,       FieldFlag.Required),
      "optByte" ->    Field(ScalaTypes.ScalaByte,       FieldFlag.Optional),
      "short" ->      Field(ScalaTypes.ScalaShort,      FieldFlag.Required),
      "optShort" ->   Field(ScalaTypes.ScalaShort,      FieldFlag.Optional),
      "int" ->        Field(ScalaTypes.ScalaInt,        FieldFlag.Required),
      "optInt" ->     Field(ScalaTypes.ScalaInt,        FieldFlag.Optional),
      "long" ->       Field(ScalaTypes.ScalaLong,       FieldFlag.Required),
      "optLong" ->    Field(ScalaTypes.ScalaLong,       FieldFlag.Optional),
      "float" ->      Field(ScalaTypes.ScalaFloat,      FieldFlag.Required),
      "optFloat" ->   Field(ScalaTypes.ScalaFloat,      FieldFlag.Optional),
      "double" ->     Field(ScalaTypes.ScalaDouble,     FieldFlag.Required),
      "optDouble" ->  Field(ScalaTypes.ScalaDouble,     FieldFlag.Optional),
      "integer" ->    Field(ScalaTypes.ScalaBigInteger, FieldFlag.Required),
      "optInteger" -> Field(ScalaTypes.ScalaBigInteger, FieldFlag.Optional),
      "decimal" ->    Field(ScalaTypes.ScalaBigDecimal, FieldFlag.Required),
      "optDecimal" -> Field(ScalaTypes.ScalaBigDecimal, FieldFlag.Optional),

      "timestamp" ->        Field(Types.Timestamp,  FieldFlag.Required),
      "optTimestamp" ->     Field(Types.Timestamp,  FieldFlag.Optional),
      "date" ->             Field(Types.Date,       FieldFlag.Required),
      "optDate" ->          Field(Types.Date,       FieldFlag.Optional),
      "time" ->             Field(Types.Time,       FieldFlag.Required),
      "optTime" ->          Field(Types.Time,       FieldFlag.Optional),

      "uuid" ->             Field(Types.UUID,       FieldFlag.Required),
      "optUuid" ->          Field(Types.UUID,       FieldFlag.Optional),
    ))
  }

  "order of fields should be honored" in {
    t.fields.head should be ("bool" ->    Field(Types.Bool, FieldFlag.Required))
    t.fields.last should be ("optUuid" -> Field(Types.UUID, FieldFlag.Optional))
  }

  "Boolean -> Bool" in {
    t.fields("bool")            should be (Field(Types.Bool,  FieldFlag.Required))
    t.fields("optBool")         should be (Field(Types.Bool,  FieldFlag.Optional))
    t.fields("defaultBool")     should be (Field(Types.Bool,  FieldFlag.WithDefault))
  }

  "String -> Text" in {
    t.fields("string")            should be (Field(ScalaTypes.ScalaString,  FieldFlag.Required))
    t.fields("optString")         should be (Field(ScalaTypes.ScalaString,  FieldFlag.Optional))
    t.fields("defaultString")     should be (Field(ScalaTypes.ScalaString,  FieldFlag.WithDefault))
    t.fields("defaultOptString")  should be (Field(ScalaTypes.ScalaString,  FieldFlag.Optional))
    t.fields("listOfStrings")     should be (Field(Types.List(ScalaTypes.ScalaString),  FieldFlag.NonEmpty))
  }

  "Byte -> Number" in {
    t.fields("byte")    should be (Field(ScalaTypes.ScalaByte, FieldFlag.Required))
    t.fields("optByte") should be (Field(ScalaTypes.ScalaByte, FieldFlag.Optional))
  }

  "Short -> Number" in {
    t.fields("short")    should be (Field(ScalaTypes.ScalaShort, FieldFlag.Required))
    t.fields("optShort") should be (Field(ScalaTypes.ScalaShort, FieldFlag.Optional))
  }

  "Int -> Number" in {
    t.fields("int")    should be (Field(ScalaTypes.ScalaInt,    FieldFlag.Required))
    t.fields("optInt") should be (Field(ScalaTypes.ScalaInt,    FieldFlag.Optional))
  }

  "Long -> Number" in {
    t.fields("long")    should be (Field(ScalaTypes.ScalaLong, FieldFlag.Required))
    t.fields("optLong") should be (Field(ScalaTypes.ScalaLong, FieldFlag.Optional))
  }

  "Float -> Number" in {
    t.fields("float")    should be (Field(ScalaTypes.ScalaFloat, FieldFlag.Required))
    t.fields("optFloat") should be (Field(ScalaTypes.ScalaFloat, FieldFlag.Optional))
  }

  "Double -> Number" in {
    t.fields("double")    should be (Field(ScalaTypes.ScalaDouble, FieldFlag.Required))
    t.fields("optDouble") should be (Field(ScalaTypes.ScalaDouble, FieldFlag.Optional))
  }

  "BigInt -> Number" in {
    t.fields("integer")    should be (Field(ScalaTypes.ScalaBigInteger, FieldFlag.Required))
    t.fields("optInteger") should be (Field(ScalaTypes.ScalaBigInteger, FieldFlag.Optional))
  }

  "BigDecimal -> Number" in {
    t.fields("decimal")    should be (Field(ScalaTypes.ScalaBigDecimal, FieldFlag.Required))
    t.fields("optDecimal") should be (Field(ScalaTypes.ScalaBigDecimal, FieldFlag.Optional))
  }

  "sql.Date -> Date" in {
    t.fields("date")         should be (Field(Types.Date,      FieldFlag.Required))
    t.fields("optDate")      should be (Field(Types.Date,      FieldFlag.Optional))
  }

  "sql.Time -> Time" in {
    t.fields("time")         should be (Field(Types.Time,      FieldFlag.Required))
    t.fields("optTime")      should be (Field(Types.Time,      FieldFlag.Optional))
  }

  "sql.Timestamp -> Timestamp" in {
    t.fields("timestamp")    should be (Field(Types.Timestamp, FieldFlag.Required))
    t.fields("optTimestamp") should be (Field(Types.Timestamp, FieldFlag.Optional))
  }

  "utils.UUID -> UUID" in {
    t.fields("uuid")    should be (Field(Types.UUID, FieldFlag.Required))
    t.fields("optUuid") should be (Field(Types.UUID, FieldFlag.Optional))
  }
}

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
