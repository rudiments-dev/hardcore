package dev.rudiments.types

import dev.rudiments.types.hard.{ScalaType, ScalaTypes}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class ValueSpecTest extends WordSpec with Matchers {
  private implicit val typeSystem: TypeSystem = new TypeSystem()
  val t: ScalaType[Example] = typeSystem.asType[Example]

  "Type transforms all basic types to BasicTypes" in {
    t.fields should be (Map(
      "bool" ->         ValueSpec(Plain.Bool, isRequired = true),
      "optBool" ->      ValueSpec(Plain.Bool, isRequired = false),
      "defaultBool" ->  ValueSpec(Plain.Bool, isRequired = false),

      "string" ->           ValueSpec(ScalaTypes.ScalaString, isRequired = true),
      "optString" ->        ValueSpec(ScalaTypes.ScalaString, isRequired = false),
      "defaultString" ->    ValueSpec(ScalaTypes.ScalaString, isRequired = false),
      "defaultOptString" -> ValueSpec(ScalaTypes.ScalaString, isRequired = false),
      "listOfStrings" ->    ValueSpec(List(ScalaTypes.ScalaString), isRequired = true),

      "byte" ->       ValueSpec(ScalaTypes.ScalaByte,       isRequired = true),
      "optByte" ->    ValueSpec(ScalaTypes.ScalaByte,       isRequired = false),
      "short" ->      ValueSpec(ScalaTypes.ScalaShort,      isRequired = true),
      "optShort" ->   ValueSpec(ScalaTypes.ScalaShort,      isRequired = false),
      "int" ->        ValueSpec(ScalaTypes.ScalaInt,        isRequired = true),
      "optInt" ->     ValueSpec(ScalaTypes.ScalaInt,        isRequired = false),
      "long" ->       ValueSpec(ScalaTypes.ScalaLong,       isRequired = true),
      "optLong" ->    ValueSpec(ScalaTypes.ScalaLong,       isRequired = false),
      "float" ->      ValueSpec(ScalaTypes.ScalaFloat,      isRequired = true),
      "optFloat" ->   ValueSpec(ScalaTypes.ScalaFloat,      isRequired = false),
      "double" ->     ValueSpec(ScalaTypes.ScalaDouble,     isRequired = true),
      "optDouble" ->  ValueSpec(ScalaTypes.ScalaDouble,     isRequired = false),
      "integer" ->    ValueSpec(ScalaTypes.ScalaBigInteger, isRequired = true),
      "optInteger" -> ValueSpec(ScalaTypes.ScalaBigInteger, isRequired = false),
      "decimal" ->    ValueSpec(ScalaTypes.ScalaBigDecimal, isRequired = true),
      "optDecimal" -> ValueSpec(ScalaTypes.ScalaBigDecimal, isRequired = false),

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
    t.fields("defaultBool")     should be (ValueSpec(Plain.Bool, isRequired = false))
  }

  "String -> Text" in {
    t.fields("string")            should be (ValueSpec(ScalaTypes.ScalaString, isRequired = true))
    t.fields("optString")         should be (ValueSpec(ScalaTypes.ScalaString, isRequired = false))
    t.fields("defaultString")     should be (ValueSpec(ScalaTypes.ScalaString, isRequired = false))
    t.fields("defaultOptString")  should be (ValueSpec(ScalaTypes.ScalaString, isRequired = false))
    t.fields("listOfStrings")     should be (ValueSpec(List(ScalaTypes.ScalaString), isRequired = true))
  }

  "Byte -> Number" in {
    t.fields("byte")    should be (ValueSpec(ScalaTypes.ScalaByte, isRequired = true))
    t.fields("optByte") should be (ValueSpec(ScalaTypes.ScalaByte, isRequired = false))
  }

  "Short -> Number" in {
    t.fields("short")    should be (ValueSpec(ScalaTypes.ScalaShort,      isRequired = true))
    t.fields("optShort") should be (ValueSpec(ScalaTypes.ScalaShort,      isRequired = false))
  }

  "Int -> Number" in {
    t.fields("int")    should be (ValueSpec(ScalaTypes.ScalaInt,        isRequired = true))
    t.fields("optInt") should be (ValueSpec(ScalaTypes.ScalaInt,        isRequired = false))
  }

  "Long -> Number" in {
    t.fields("long")    should be (ValueSpec(ScalaTypes.ScalaLong,       isRequired = true))
    t.fields("optLong") should be (ValueSpec(ScalaTypes.ScalaLong,       isRequired = false))
  }

  "Float -> Number" in {
    t.fields("float")    should be (ValueSpec(ScalaTypes.ScalaFloat,      isRequired = true))
    t.fields("optFloat") should be (ValueSpec(ScalaTypes.ScalaFloat,      isRequired = false))
  }

  "Double -> Number" in {
    t.fields("double")    should be (ValueSpec(ScalaTypes.ScalaDouble,     isRequired = true))
    t.fields("optDouble") should be (ValueSpec(ScalaTypes.ScalaDouble,     isRequired = false))
  }

  "BigInt -> Number" in {
    t.fields("integer")    should be (ValueSpec(ScalaTypes.ScalaBigInteger, isRequired = true))
    t.fields("optInteger") should be (ValueSpec(ScalaTypes.ScalaBigInteger, isRequired = false))
  }

  "BigDecimal -> Number" in {
    t.fields("decimal")    should be (ValueSpec(ScalaTypes.ScalaBigDecimal, isRequired = true))
    t.fields("optDecimal") should be (ValueSpec(ScalaTypes.ScalaBigDecimal, isRequired = false))
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
