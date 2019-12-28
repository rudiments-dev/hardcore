package dev.rudiments.hardcore.types

import java.sql.{Date, Time, Timestamp}

import org.junit.runner.RunWith
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FieldSpec extends WordSpec with Matchers {
  val t: Type[Example] = Type[Example]

  "Type transforms all basic types to BasicTypes" in {
    t.fields should be (Map(
      "string" ->           Field(RudimentTypes.Text,       FieldFlags.Required),
      "optString" ->        Field(RudimentTypes.Text,       FieldFlags.Optional),
      "defaultString" ->    Field(RudimentTypes.Text,       FieldFlags.WithDefault),
      "defaultOptString" -> Field(RudimentTypes.Text,       FieldFlags.Optional),
      "listOfStrings" ->    Field(RudimentTypes.List(RudimentTypes.Text), CollectionFlags.CanBeEmpty),
      "int" ->              Field(RudimentTypes.Number,     FieldFlags.Required),
      "optInt" ->           Field(RudimentTypes.Number,     FieldFlags.Optional),
      "double" ->           Field(RudimentTypes.Number,     FieldFlags.Required),
      "optDouble" ->        Field(RudimentTypes.Number,     FieldFlags.Optional),
      "long" ->             Field(RudimentTypes.Number,     FieldFlags.Required),
      "optLong" ->          Field(RudimentTypes.Number,     FieldFlags.Optional),
      "decimal" ->          Field(RudimentTypes.Number,     FieldFlags.Required),
      "optDecimal" ->       Field(RudimentTypes.Number,     FieldFlags.Optional),
      "timestamp" ->        Field(RudimentTypes.Timestamp,  FieldFlags.Required),
      "optTimestamp" ->     Field(RudimentTypes.Timestamp,  FieldFlags.Optional),
      "date" ->             Field(RudimentTypes.Date,       FieldFlags.Required),
      "optDate" ->          Field(RudimentTypes.Date,       FieldFlags.Optional),
      "time" ->             Field(RudimentTypes.Time,       FieldFlags.Required),
      "optTime" ->          Field(RudimentTypes.Time,       FieldFlags.Optional),
    ))
  }

  "String -> Text" in {
    t.fields("string")            should be (Field(RudimentTypes.Text,  FieldFlags.Required))
    t.fields("optString")         should be (Field(RudimentTypes.Text,  FieldFlags.Optional))
    t.fields("defaultString")     should be (Field(RudimentTypes.Text,  FieldFlags.WithDefault))
    t.fields("defaultOptString")  should be (Field(RudimentTypes.Text,  FieldFlags.Optional))
    t.fields("listOfStrings")     should be (Field(RudimentTypes.List(RudimentTypes.Text),  CollectionFlags.CanBeEmpty))
  }

  "Int -> Number" in {
    t.fields("int")          should be (Field(RudimentTypes.Number,    FieldFlags.Required))
    t.fields("optInt")       should be (Field(RudimentTypes.Number,    FieldFlags.Optional))
  }

  "Double -> Number" in {
    t.fields("double")       should be (Field(RudimentTypes.Number,    FieldFlags.Required))
    t.fields("optDouble")    should be (Field(RudimentTypes.Number,    FieldFlags.Optional))
  }

  "Long -> Number" in {
    t.fields("long")         should be (Field(RudimentTypes.Number,    FieldFlags.Required))
    t.fields("optLong")      should be (Field(RudimentTypes.Number,    FieldFlags.Optional))
  }

  "BigDecimal -> Number" in {
    t.fields("decimal")      should be (Field(RudimentTypes.Number,    FieldFlags.Required))
    t.fields("optDecimal")   should be (Field(RudimentTypes.Number,    FieldFlags.Optional))
  }

  "sql.Date -> Date" in {
    t.fields("date")         should be (Field(RudimentTypes.Date,      FieldFlags.Required))
    t.fields("optDate")      should be (Field(RudimentTypes.Date,      FieldFlags.Optional))
  }

  "sql.Time -> Time" in {
    t.fields("time")         should be (Field(RudimentTypes.Time,      FieldFlags.Required))
    t.fields("optTime")      should be (Field(RudimentTypes.Time,      FieldFlags.Optional))
  }

  "sql.Timestamp -> Timestamp" in {
    t.fields("timestamp")    should be (Field(RudimentTypes.Timestamp, FieldFlags.Required))
    t.fields("optTimestamp") should be (Field(RudimentTypes.Timestamp, FieldFlags.Optional))
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
