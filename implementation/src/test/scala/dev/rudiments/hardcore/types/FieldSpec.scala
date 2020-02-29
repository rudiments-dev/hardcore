package dev.rudiments.hardcore.types

import java.sql.{Date, Time, Timestamp}

import org.junit.runner.RunWith
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FieldSpec extends WordSpec with Matchers {
  val t: HardType[Example] = HardType[Example]

  "Type transforms all basic types to BasicTypes" in {
    t.fields should be (Map(
      "string" ->           Field(RudimentTypes.Text.ScalaString, FieldFlag.Required),
      "optString" ->        Field(RudimentTypes.Text.ScalaString, FieldFlag.Optional),
      "defaultString" ->    Field(RudimentTypes.Text.ScalaString, FieldFlag.WithDefault),
      "defaultOptString" -> Field(RudimentTypes.Text.ScalaString, FieldFlag.Optional),
      "listOfStrings" ->    Field(RudimentTypes.List(RudimentTypes.Text.ScalaString), FieldFlag.NonEmpty),

      "byte" ->       Field(RudimentTypes.Number.ScalaByte,       FieldFlag.Required),
      "optByte" ->    Field(RudimentTypes.Number.ScalaByte,       FieldFlag.Optional),
      "short" ->      Field(RudimentTypes.Number.ScalaShort,      FieldFlag.Required),
      "optShort" ->   Field(RudimentTypes.Number.ScalaShort,      FieldFlag.Optional),
      "int" ->        Field(RudimentTypes.Number.ScalaInt,        FieldFlag.Required),
      "optInt" ->     Field(RudimentTypes.Number.ScalaInt,        FieldFlag.Optional),
      "long" ->       Field(RudimentTypes.Number.ScalaLong,       FieldFlag.Required),
      "optLong" ->    Field(RudimentTypes.Number.ScalaLong,       FieldFlag.Optional),
      "float" ->      Field(RudimentTypes.Number.ScalaFloat,      FieldFlag.Required),
      "optFloat" ->   Field(RudimentTypes.Number.ScalaFloat,      FieldFlag.Optional),
      "double" ->     Field(RudimentTypes.Number.ScalaDouble,     FieldFlag.Required),
      "optDouble" ->  Field(RudimentTypes.Number.ScalaDouble,     FieldFlag.Optional),
      "integer" ->    Field(RudimentTypes.Number.ScalaBigInteger, FieldFlag.Required),
      "optInteger" -> Field(RudimentTypes.Number.ScalaBigInteger, FieldFlag.Optional),
      "decimal" ->    Field(RudimentTypes.Number.ScalaBigDecimal, FieldFlag.Required),
      "optDecimal" -> Field(RudimentTypes.Number.ScalaBigDecimal, FieldFlag.Optional),

      "timestamp" ->        Field(RudimentTypes.Timestamp,  FieldFlag.Required),
      "optTimestamp" ->     Field(RudimentTypes.Timestamp,  FieldFlag.Optional),
      "date" ->             Field(RudimentTypes.Date,       FieldFlag.Required),
      "optDate" ->          Field(RudimentTypes.Date,       FieldFlag.Optional),
      "time" ->             Field(RudimentTypes.Time,       FieldFlag.Required),
      "optTime" ->          Field(RudimentTypes.Time,       FieldFlag.Optional),
    ))
  }

  "order of fields should be honored" in {
    t.fields.head should be ("string" -> Field(RudimentTypes.Text.ScalaString,  FieldFlag.Required))
    t.fields.last should be ("optTime" -> Field(RudimentTypes.Time, FieldFlag.Optional))
  }

  "String -> Text" in {
    t.fields("string")            should be (Field(RudimentTypes.Text.ScalaString,  FieldFlag.Required))
    t.fields("optString")         should be (Field(RudimentTypes.Text.ScalaString,  FieldFlag.Optional))
    t.fields("defaultString")     should be (Field(RudimentTypes.Text.ScalaString,  FieldFlag.WithDefault))
    t.fields("defaultOptString")  should be (Field(RudimentTypes.Text.ScalaString,  FieldFlag.Optional))
    t.fields("listOfStrings")     should be (Field(RudimentTypes.List(RudimentTypes.Text.ScalaString),  FieldFlag.NonEmpty))
  }

  "Byte -> Number" in {
    t.fields("byte")    should be (Field(RudimentTypes.Number.ScalaByte, FieldFlag.Required))
    t.fields("optByte") should be (Field(RudimentTypes.Number.ScalaByte, FieldFlag.Optional))
  }

  "Short -> Number" in {
    t.fields("short")    should be (Field(RudimentTypes.Number.ScalaShort, FieldFlag.Required))
    t.fields("optShort") should be (Field(RudimentTypes.Number.ScalaShort, FieldFlag.Optional))
  }

  "Int -> Number" in {
    t.fields("int")    should be (Field(RudimentTypes.Number.ScalaInt,    FieldFlag.Required))
    t.fields("optInt") should be (Field(RudimentTypes.Number.ScalaInt,    FieldFlag.Optional))
  }

  "Long -> Number" in {
    t.fields("long")    should be (Field(RudimentTypes.Number.ScalaLong, FieldFlag.Required))
    t.fields("optLong") should be (Field(RudimentTypes.Number.ScalaLong, FieldFlag.Optional))
  }

  "Float -> Number" in {
    t.fields("float")    should be (Field(RudimentTypes.Number.ScalaFloat, FieldFlag.Required))
    t.fields("optFloat") should be (Field(RudimentTypes.Number.ScalaFloat, FieldFlag.Optional))
  }

  "Double -> Number" in {
    t.fields("double")    should be (Field(RudimentTypes.Number.ScalaDouble, FieldFlag.Required))
    t.fields("optDouble") should be (Field(RudimentTypes.Number.ScalaDouble, FieldFlag.Optional))
  }

  "BigInt -> Number" in {
    t.fields("integer")    should be (Field(RudimentTypes.Number.ScalaBigInteger, FieldFlag.Required))
    t.fields("optInteger") should be (Field(RudimentTypes.Number.ScalaBigInteger, FieldFlag.Optional))
  }

  "BigDecimal -> Number" in {
    t.fields("decimal")    should be (Field(RudimentTypes.Number.ScalaBigDecimal, FieldFlag.Required))
    t.fields("optDecimal") should be (Field(RudimentTypes.Number.ScalaBigDecimal, FieldFlag.Optional))
  }

  "sql.Date -> Date" in {
    t.fields("date")         should be (Field(RudimentTypes.Date,      FieldFlag.Required))
    t.fields("optDate")      should be (Field(RudimentTypes.Date,      FieldFlag.Optional))
  }

  "sql.Time -> Time" in {
    t.fields("time")         should be (Field(RudimentTypes.Time,      FieldFlag.Required))
    t.fields("optTime")      should be (Field(RudimentTypes.Time,      FieldFlag.Optional))
  }

  "sql.Timestamp -> Timestamp" in {
    t.fields("timestamp")    should be (Field(RudimentTypes.Timestamp, FieldFlag.Required))
    t.fields("optTimestamp") should be (Field(RudimentTypes.Timestamp, FieldFlag.Optional))
  }
}

case class Example(
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
  optTime: Option[Time]
) extends DTO
