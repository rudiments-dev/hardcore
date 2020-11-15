package dev.rudiments.domain

import dev.rudiments.domain.ScalaTypes._
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ValueSpecTest extends AnyWordSpec with Matchers {
  private implicit val domain: Domain = Domain()
  val t: Spec = domain.makeFromScala[Spec, Example]

  "Type transforms all basic types to BasicTypes" in {
    t.fields should be (Map(
      "bool" ->         ValueSpec(Plain.Bool, isRequired = true),
      "optBool" ->      ValueSpec(Plain.Bool, isRequired = false),
      "defaultBool" ->  ValueSpec(Plain.Bool, isRequired = true),

      "string" ->           ValueSpec(ScalaString, isRequired = true),
      "optString" ->        ValueSpec(ScalaString, isRequired = false),
      "defaultString" ->    ValueSpec(ScalaString, isRequired = true),
      "defaultOptString" -> ValueSpec(ScalaString, isRequired = false),
      "listOfStrings" ->    ValueSpec(List(ScalaString), isRequired = true),

      "byte" ->       ValueSpec(ScalaByte,       isRequired = true),
      "optByte" ->    ValueSpec(ScalaByte,       isRequired = false),
      "short" ->      ValueSpec(ScalaShort,      isRequired = true),
      "optShort" ->   ValueSpec(ScalaShort,      isRequired = false),
      "int" ->        ValueSpec(ScalaInt,        isRequired = true),
      "optInt" ->     ValueSpec(ScalaInt,        isRequired = false),
      "long" ->       ValueSpec(ScalaLong,       isRequired = true),
      "optLong" ->    ValueSpec(ScalaLong,       isRequired = false),
      "float" ->      ValueSpec(ScalaFloat,      isRequired = true),
      "optFloat" ->   ValueSpec(ScalaFloat,      isRequired = false),
      "double" ->     ValueSpec(ScalaDouble,     isRequired = true),
      "optDouble" ->  ValueSpec(ScalaDouble,     isRequired = false),
      "integer" ->    ValueSpec(ScalaBigInteger, isRequired = true),
      "optInteger" -> ValueSpec(ScalaBigInteger, isRequired = false),
      "decimal" ->    ValueSpec(ScalaBigDecimal, isRequired = true),
      "optDecimal" -> ValueSpec(ScalaBigDecimal, isRequired = false),

      "timestamp" ->        ValueSpec(Plain.Timestamp,  isRequired = true),
      "optTimestamp" ->     ValueSpec(Plain.Timestamp,  isRequired = false),
      "date" ->             ValueSpec(Plain.Date,       isRequired = true),
      "optDate" ->          ValueSpec(Plain.Date,       isRequired = false),
      "time" ->             ValueSpec(Plain.Time,       isRequired = true),
      "optTime" ->          ValueSpec(Plain.Time,       isRequired = false),

      "uuid" ->     ValueSpec(Plain.UUID, isRequired = true),
      "optUuid" ->  ValueSpec(Plain.UUID, isRequired = false),
    ))
  }

  "order of fields should be honored" in {
    t.fields.head should be ("bool" ->    ValueSpec(Plain.Bool, isRequired = true))
    t.fields.last should be ("optUuid" -> ValueSpec(Plain.UUID, isRequired = false))
  }

  "Boolean -> Bool" in {
    t.fields("bool")            should be (ValueSpec(Plain.Bool, isRequired = true))
    t.fields("optBool")         should be (ValueSpec(Plain.Bool, isRequired = false))
    t.fields("defaultBool")     should be (ValueSpec(Plain.Bool, isRequired = true))
  }

  "String -> Text" in {
    t.fields("string")            should be (ValueSpec(ScalaString, isRequired = true))
    t.fields("optString")         should be (ValueSpec(ScalaString, isRequired = false))
    t.fields("defaultString")     should be (ValueSpec(ScalaString, isRequired = true))
    t.fields("defaultOptString")  should be (ValueSpec(ScalaString, isRequired = false))
    t.fields("listOfStrings")     should be (ValueSpec(List(ScalaString), isRequired = true))
  }

  "Byte -> Number" in {
    t.fields("byte")    should be (ValueSpec(ScalaByte, isRequired = true))
    t.fields("optByte") should be (ValueSpec(ScalaByte, isRequired = false))
  }

  "Short -> Number" in {
    t.fields("short")    should be (ValueSpec(ScalaShort,      isRequired = true))
    t.fields("optShort") should be (ValueSpec(ScalaShort,      isRequired = false))
  }

  "Int -> Number" in {
    t.fields("int")    should be (ValueSpec(ScalaInt,        isRequired = true))
    t.fields("optInt") should be (ValueSpec(ScalaInt,        isRequired = false))
  }

  "Long -> Number" in {
    t.fields("long")    should be (ValueSpec(ScalaLong,       isRequired = true))
    t.fields("optLong") should be (ValueSpec(ScalaLong,       isRequired = false))
  }

  "Float -> Number" in {
    t.fields("float")    should be (ValueSpec(ScalaFloat,      isRequired = true))
    t.fields("optFloat") should be (ValueSpec(ScalaFloat,      isRequired = false))
  }

  "Double -> Number" in {
    t.fields("double")    should be (ValueSpec(ScalaDouble,     isRequired = true))
    t.fields("optDouble") should be (ValueSpec(ScalaDouble,     isRequired = false))
  }

  "BigInt -> Number" in {
    t.fields("integer")    should be (ValueSpec(ScalaBigInteger, isRequired = true))
    t.fields("optInteger") should be (ValueSpec(ScalaBigInteger, isRequired = false))
  }

  "BigDecimal -> Number" in {
    t.fields("decimal")    should be (ValueSpec(ScalaBigDecimal, isRequired = true))
    t.fields("optDecimal") should be (ValueSpec(ScalaBigDecimal, isRequired = false))
  }

  "sql.Date -> Date" in {
    t.fields("date")         should be (ValueSpec(Plain.Date,       isRequired = true))
    t.fields("optDate")      should be (ValueSpec(Plain.Date,       isRequired = false))
  }

  "sql.Time -> Time" in {
    t.fields("time")         should be (ValueSpec(Plain.Time,       isRequired = true))
    t.fields("optTime")      should be (ValueSpec(Plain.Time,       isRequired = false))
  }

  "sql.Timestamp -> Timestamp" in {
    t.fields("timestamp")    should be (ValueSpec(Plain.Timestamp,  isRequired = true))
    t.fields("optTimestamp") should be (ValueSpec(Plain.Timestamp,  isRequired = false))
  }

  "utils.UUID -> UUID" in {
    t.fields("uuid")    should be (ValueSpec(Plain.UUID, isRequired = true))
    t.fields("optUuid") should be (ValueSpec(Plain.UUID, isRequired = false))
  }
}
