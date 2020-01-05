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
      "string" ->           Field(RudimentTypes.Text,       FieldFlag.Required),
      "optString" ->        Field(RudimentTypes.Text,       FieldFlag.Optional),
      "defaultString" ->    Field(RudimentTypes.Text,       FieldFlag.WithDefault),
      "defaultOptString" -> Field(RudimentTypes.Text,       FieldFlag.Optional),
      "listOfStrings" ->    Field(RudimentTypes.List(RudimentTypes.Text), FieldFlag.NonEmpty),
      "int" ->              Field(RudimentTypes.Number,     FieldFlag.Required),
      "optInt" ->           Field(RudimentTypes.Number,     FieldFlag.Optional),
      "double" ->           Field(RudimentTypes.Number,     FieldFlag.Required),
      "optDouble" ->        Field(RudimentTypes.Number,     FieldFlag.Optional),
      "long" ->             Field(RudimentTypes.Number,     FieldFlag.Required),
      "optLong" ->          Field(RudimentTypes.Number,     FieldFlag.Optional),
      "decimal" ->          Field(RudimentTypes.Number,     FieldFlag.Required),
      "optDecimal" ->       Field(RudimentTypes.Number,     FieldFlag.Optional),
      "timestamp" ->        Field(RudimentTypes.Timestamp,  FieldFlag.Required),
      "optTimestamp" ->     Field(RudimentTypes.Timestamp,  FieldFlag.Optional),
      "date" ->             Field(RudimentTypes.Date,       FieldFlag.Required),
      "optDate" ->          Field(RudimentTypes.Date,       FieldFlag.Optional),
      "time" ->             Field(RudimentTypes.Time,       FieldFlag.Required),
      "optTime" ->          Field(RudimentTypes.Time,       FieldFlag.Optional),
    ))
  }

  "order of fields should be honored" in {
    t.fields.head should be ("string" -> Field(RudimentTypes.Text,  FieldFlag.Required))
    t.fields.last should be ("optTime" -> Field(RudimentTypes.Time, FieldFlag.Optional))
  }

  "String -> Text" in {
    t.fields("string")            should be (Field(RudimentTypes.Text,  FieldFlag.Required))
    t.fields("optString")         should be (Field(RudimentTypes.Text,  FieldFlag.Optional))
    t.fields("defaultString")     should be (Field(RudimentTypes.Text,  FieldFlag.WithDefault))
    t.fields("defaultOptString")  should be (Field(RudimentTypes.Text,  FieldFlag.Optional))
    t.fields("listOfStrings")     should be (Field(RudimentTypes.List(RudimentTypes.Text),  FieldFlag.NonEmpty))
  }

  "Int -> Number" in {
    t.fields("int")          should be (Field(RudimentTypes.Number,    FieldFlag.Required))
    t.fields("optInt")       should be (Field(RudimentTypes.Number,    FieldFlag.Optional))
  }

  "Double -> Number" in {
    t.fields("double")       should be (Field(RudimentTypes.Number,    FieldFlag.Required))
    t.fields("optDouble")    should be (Field(RudimentTypes.Number,    FieldFlag.Optional))
  }

  "Long -> Number" in {
    t.fields("long")         should be (Field(RudimentTypes.Number,    FieldFlag.Required))
    t.fields("optLong")      should be (Field(RudimentTypes.Number,    FieldFlag.Optional))
  }

  "BigDecimal -> Number" in {
    t.fields("decimal")      should be (Field(RudimentTypes.Number,    FieldFlag.Required))
    t.fields("optDecimal")   should be (Field(RudimentTypes.Number,    FieldFlag.Optional))
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
  int: Int,
  optInt: Option[Int],
  double: Double,
  optDouble: Option[Double],
  long: Long,
  optLong: Option[Long],
  decimal: BigDecimal,
  optDecimal: Option[BigDecimal],
  timestamp: Timestamp,
  optTimestamp: Option[Timestamp],
  date: Date,
  optDate: Option[Date],
  time: Time,
  optTime: Option[Time]
) extends DTO
