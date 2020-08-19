package dev.rudiments.types

import dev.rudiments.types.hard.{ScalaType, ScalaTypes}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class FieldSpec extends WordSpec with Matchers {
  private implicit val typeSystem: TypeSystem = new TypeSystem()
  val t: ScalaType[Example] = typeSystem.asType[Example]

  "Type transforms all basic types to BasicTypes" in {
    t.fields should be (Map(
      "bool" ->         Field(Plain.Bool, isRequired = true),
      "optBool" ->      Field(Plain.Bool, isRequired = false),
      "defaultBool" ->  Field(Plain.Bool, isRequired = false),

      "string" ->           Field(ScalaTypes.ScalaString, isRequired = true),
      "optString" ->        Field(ScalaTypes.ScalaString, isRequired = false),
      "defaultString" ->    Field(ScalaTypes.ScalaString, isRequired = false),
      "defaultOptString" -> Field(ScalaTypes.ScalaString, isRequired = false),
      "listOfStrings" ->    Field(List(ScalaTypes.ScalaString), isRequired = true),

      "byte" ->       Field(ScalaTypes.ScalaByte,       isRequired = true),
      "optByte" ->    Field(ScalaTypes.ScalaByte,       isRequired = false),
      "short" ->      Field(ScalaTypes.ScalaShort,      isRequired = true),
      "optShort" ->   Field(ScalaTypes.ScalaShort,      isRequired = false),
      "int" ->        Field(ScalaTypes.ScalaInt,        isRequired = true),
      "optInt" ->     Field(ScalaTypes.ScalaInt,        isRequired = false),
      "long" ->       Field(ScalaTypes.ScalaLong,       isRequired = true),
      "optLong" ->    Field(ScalaTypes.ScalaLong,       isRequired = false),
      "float" ->      Field(ScalaTypes.ScalaFloat,      isRequired = true),
      "optFloat" ->   Field(ScalaTypes.ScalaFloat,      isRequired = false),
      "double" ->     Field(ScalaTypes.ScalaDouble,     isRequired = true),
      "optDouble" ->  Field(ScalaTypes.ScalaDouble,     isRequired = false),
      "integer" ->    Field(ScalaTypes.ScalaBigInteger, isRequired = true),
      "optInteger" -> Field(ScalaTypes.ScalaBigInteger, isRequired = false),
      "decimal" ->    Field(ScalaTypes.ScalaBigDecimal, isRequired = true),
      "optDecimal" -> Field(ScalaTypes.ScalaBigDecimal, isRequired = false),

      "timestamp" ->        Field(Plain.Timestamp,  isRequired = true),
      "optTimestamp" ->     Field(Plain.Timestamp,  isRequired = false),
      "date" ->             Field(Plain.Date,       isRequired = true),
      "optDate" ->          Field(Plain.Date,       isRequired = false),
      "time" ->             Field(Plain.Time,       isRequired = true),
      "optTime" ->          Field(Plain.Time,       isRequired = false),

      "uuid" ->     Field(Plain.UUID, isRequired = true),
      "optUuid" ->  Field(Plain.UUID, isRequired = false),
    ))
  }

  "order of fields should be honored" in {
    t.fields.head should be ("bool" ->    Field(Plain.Bool, isRequired = true))
    t.fields.last should be ("optUuid" -> Field(Plain.UUID, isRequired = false))
  }

  "Boolean -> Bool" in {
    t.fields("bool")            should be (Field(Plain.Bool, isRequired = true))
    t.fields("optBool")         should be (Field(Plain.Bool, isRequired = false))
    t.fields("defaultBool")     should be (Field(Plain.Bool, isRequired = false))
  }

  "String -> Text" in {
    t.fields("string")            should be (Field(ScalaTypes.ScalaString, isRequired = true))
    t.fields("optString")         should be (Field(ScalaTypes.ScalaString, isRequired = false))
    t.fields("defaultString")     should be (Field(ScalaTypes.ScalaString, isRequired = false))
    t.fields("defaultOptString")  should be (Field(ScalaTypes.ScalaString, isRequired = false))
    t.fields("listOfStrings")     should be (Field(List(ScalaTypes.ScalaString), isRequired = true))
  }

  "Byte -> Number" in {
    t.fields("byte")    should be (Field(ScalaTypes.ScalaByte, isRequired = true))
    t.fields("optByte") should be (Field(ScalaTypes.ScalaByte, isRequired = false))
  }

  "Short -> Number" in {
    t.fields("short")    should be (Field(ScalaTypes.ScalaShort,      isRequired = true))
    t.fields("optShort") should be (Field(ScalaTypes.ScalaShort,      isRequired = false))
  }

  "Int -> Number" in {
    t.fields("int")    should be (Field(ScalaTypes.ScalaInt,        isRequired = true))
    t.fields("optInt") should be (Field(ScalaTypes.ScalaInt,        isRequired = false))
  }

  "Long -> Number" in {
    t.fields("long")    should be (Field(ScalaTypes.ScalaLong,       isRequired = true))
    t.fields("optLong") should be (Field(ScalaTypes.ScalaLong,       isRequired = false))
  }

  "Float -> Number" in {
    t.fields("float")    should be (Field(ScalaTypes.ScalaFloat,      isRequired = true))
    t.fields("optFloat") should be (Field(ScalaTypes.ScalaFloat,      isRequired = false))
  }

  "Double -> Number" in {
    t.fields("double")    should be (Field(ScalaTypes.ScalaDouble,     isRequired = true))
    t.fields("optDouble") should be (Field(ScalaTypes.ScalaDouble,     isRequired = false))
  }

  "BigInt -> Number" in {
    t.fields("integer")    should be (Field(ScalaTypes.ScalaBigInteger, isRequired = true))
    t.fields("optInteger") should be (Field(ScalaTypes.ScalaBigInteger, isRequired = false))
  }

  "BigDecimal -> Number" in {
    t.fields("decimal")    should be (Field(ScalaTypes.ScalaBigDecimal, isRequired = true))
    t.fields("optDecimal") should be (Field(ScalaTypes.ScalaBigDecimal, isRequired = false))
  }

  "sql.Date -> Date" in {
    t.fields("date")         should be (Field(Plain.Date,       isRequired = true))
    t.fields("optDate")      should be (Field(Plain.Date,       isRequired = false))
  }

  "sql.Time -> Time" in {
    t.fields("time")         should be (Field(Plain.Time,       isRequired = true))
    t.fields("optTime")      should be (Field(Plain.Time,       isRequired = false))
  }

  "sql.Timestamp -> Timestamp" in {
    t.fields("timestamp")    should be (Field(Plain.Timestamp,  isRequired = true))
    t.fields("optTimestamp") should be (Field(Plain.Timestamp,  isRequired = false))
  }

  "utils.UUID -> UUID" in {
    t.fields("uuid")    should be (Field(Plain.UUID, isRequired = true))
    t.fields("optUuid") should be (Field(Plain.UUID, isRequired = false))
  }
}
