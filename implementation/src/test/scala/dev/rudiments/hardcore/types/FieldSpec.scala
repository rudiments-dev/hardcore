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
      "string" ->       Field(BasicTypes.Text,      false, false),
      "optString" ->    Field(BasicTypes.Text,      true,  false),
      "int" ->          Field(BasicTypes.Number,    false, false),
      "optInt" ->       Field(BasicTypes.Number,    true,  false),
      "double" ->       Field(BasicTypes.Number,    false, false),
      "optDouble" ->    Field(BasicTypes.Number,    true,  false),
      "long" ->         Field(BasicTypes.Number,    false, false),
      "optLong" ->      Field(BasicTypes.Number,    true,  false),
      "decimal" ->      Field(BasicTypes.Number,    false, false),
      "optDecimal" ->   Field(BasicTypes.Number,    true,  false),
      "timestamp" ->    Field(BasicTypes.Timestamp, false, false),
      "optTimestamp" -> Field(BasicTypes.Timestamp, true,  false),
      "date" ->         Field(BasicTypes.Date,      false, false),
      "optDate" ->      Field(BasicTypes.Date,      true,  false),
      "time" ->         Field(BasicTypes.Time,      false, false),
      "optTime" ->      Field(BasicTypes.Time,      true,  false),
    ))
  }

  "String -> Text" in {
    t.fields("string")       should be (Field(BasicTypes.Text,      false, false))
    t.fields("optString")    should be (Field(BasicTypes.Text,      true,  false))
  }

  "Int -> Number" in {
    t.fields("int")          should be (Field(BasicTypes.Number,    false, false))
    t.fields("optInt")       should be (Field(BasicTypes.Number,    true,  false))
  }

  "Double -> Number" in {
    t.fields("double")       should be (Field(BasicTypes.Number,    false, false))
    t.fields("optDouble")    should be (Field(BasicTypes.Number,    true,  false))
  }

  "Long -> Number" in {
    t.fields("long")         should be (Field(BasicTypes.Number,    false, false))
    t.fields("optLong")      should be (Field(BasicTypes.Number,    true,  false))
  }

  "BigDecimal -> Number" in {
    t.fields("decimal")      should be (Field(BasicTypes.Number,    false, false))
    t.fields("optDecimal")   should be (Field(BasicTypes.Number,    true,  false))
  }

  "sql.Timestamp -> Timestamp" in {
    t.fields("timestamp")    should be (Field(BasicTypes.Timestamp, false, false))
    t.fields("optTimestamp") should be (Field(BasicTypes.Timestamp, true,  false))
  }

  "sql.Date -> Date" in {
    t.fields("date")         should be (Field(BasicTypes.Date,      false, false))
    t.fields("optDate")      should be (Field(BasicTypes.Date,      true,  false))
    t.fields("time")         should be (Field(BasicTypes.Time,      false, false))
    t.fields("optTime")      should be (Field(BasicTypes.Time,      true,  false))
  }

  "sql.Time -> Time" in {
    t.fields("time")         should be (Field(BasicTypes.Time,      false, false))
    t.fields("optTime")      should be (Field(BasicTypes.Time,      true,  false))
  }
}

case class Example(
  string: String,
  optString: Option[String],
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
