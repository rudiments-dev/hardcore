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
      "bool" ->         Field("bool", Plain.Bool, isRequired = true),
      "optBool" ->      Field("optBool", Plain.Bool, isRequired = false),
      "defaultBool" ->  Field("defaultBool", Plain.Bool, isRequired = false),

      "string" ->           Field("string", ScalaTypes.ScalaString, isRequired = true),
      "optString" ->        Field("optString", ScalaTypes.ScalaString, isRequired = false),
      "defaultString" ->    Field("defaultString", ScalaTypes.ScalaString, isRequired = false),
      "defaultOptString" -> Field("defaultOptString", ScalaTypes.ScalaString, isRequired = false),
      "listOfStrings" ->    Field("listOfStrings", List(ScalaTypes.ScalaString), isRequired = true),

      "byte" ->       Field("byte",       ScalaTypes.ScalaByte,       isRequired = true),
      "optByte" ->    Field("optByte",    ScalaTypes.ScalaByte,       isRequired = false),
      "short" ->      Field("short",      ScalaTypes.ScalaShort,      isRequired = true),
      "optShort" ->   Field("optShort",   ScalaTypes.ScalaShort,      isRequired = false),
      "int" ->        Field("int",        ScalaTypes.ScalaInt,        isRequired = true),
      "optInt" ->     Field("optInt",     ScalaTypes.ScalaInt,        isRequired = false),
      "long" ->       Field("long",       ScalaTypes.ScalaLong,       isRequired = true),
      "optLong" ->    Field("optLong",    ScalaTypes.ScalaLong,       isRequired = false),
      "float" ->      Field("float",      ScalaTypes.ScalaFloat,      isRequired = true),
      "optFloat" ->   Field("optFloat",   ScalaTypes.ScalaFloat,      isRequired = false),
      "double" ->     Field("double",     ScalaTypes.ScalaDouble,     isRequired = true),
      "optDouble" ->  Field("optDouble",  ScalaTypes.ScalaDouble,     isRequired = false),
      "integer" ->    Field("integer",    ScalaTypes.ScalaBigInteger, isRequired = true),
      "optInteger" -> Field("optInteger", ScalaTypes.ScalaBigInteger, isRequired = false),
      "decimal" ->    Field("decimal",    ScalaTypes.ScalaBigDecimal, isRequired = true),
      "optDecimal" -> Field("optDecimal", ScalaTypes.ScalaBigDecimal, isRequired = false),

      "timestamp" ->        Field("timestamp",    Plain.Timestamp,  isRequired = true),
      "optTimestamp" ->     Field("optTimestamp", Plain.Timestamp,  isRequired = false),
      "date" ->             Field("date",         Plain.Date,       isRequired = true),
      "optDate" ->          Field("optDate",      Plain.Date,       isRequired = false),
      "time" ->             Field("time",         Plain.Time,       isRequired = true),
      "optTime" ->          Field("optTime",      Plain.Time,       isRequired = false),

      "uuid" ->     Field("uuid",     Plain.UUID, isRequired = true),
      "optUuid" ->  Field("optUuid",  Plain.UUID, isRequired = false),
    ))
  }

  "order of fields should be honored" in {
    t.fields.head should be ("bool" ->    Field("bool",     Plain.Bool, isRequired = true))
    t.fields.last should be ("optUuid" -> Field("optUuid",  Plain.UUID, isRequired = false))
  }

  "Boolean -> Bool" in {
    t.fields("bool")            should be (Field("bool",        Plain.Bool, isRequired = true))
    t.fields("optBool")         should be (Field("optBool",     Plain.Bool, isRequired = false))
    t.fields("defaultBool")     should be (Field("defaultBool", Plain.Bool, isRequired = false))
  }

  "String -> Text" in {
    t.fields("string")            should be (Field("string", ScalaTypes.ScalaString, isRequired = true))
    t.fields("optString")         should be (Field("optString", ScalaTypes.ScalaString, isRequired = false))
    t.fields("defaultString")     should be (Field("defaultString", ScalaTypes.ScalaString, isRequired = false))
    t.fields("defaultOptString")  should be (Field("defaultOptString", ScalaTypes.ScalaString, isRequired = false))
    t.fields("listOfStrings")     should be (Field("listOfStrings", List(ScalaTypes.ScalaString), isRequired = true))
  }

  "Byte -> Number" in {
    t.fields("byte")    should be (Field("byte",    ScalaTypes.ScalaByte, isRequired = true))
    t.fields("optByte") should be (Field("optByte", ScalaTypes.ScalaByte, isRequired = false))
  }

  "Short -> Number" in {
    t.fields("short")    should be (Field("short",      ScalaTypes.ScalaShort,      isRequired = true))
    t.fields("optShort") should be (Field("optShort",   ScalaTypes.ScalaShort,      isRequired = false))
  }

  "Int -> Number" in {
    t.fields("int")    should be (Field("int",        ScalaTypes.ScalaInt,        isRequired = true))
    t.fields("optInt") should be (Field("optInt",     ScalaTypes.ScalaInt,        isRequired = false))
  }

  "Long -> Number" in {
    t.fields("long")    should be (Field("long",       ScalaTypes.ScalaLong,       isRequired = true))
    t.fields("optLong") should be (Field("optLong",    ScalaTypes.ScalaLong,       isRequired = false))
  }

  "Float -> Number" in {
    t.fields("float")    should be (Field("float",      ScalaTypes.ScalaFloat,      isRequired = true))
    t.fields("optFloat") should be (Field("optFloat",   ScalaTypes.ScalaFloat,      isRequired = false))
  }

  "Double -> Number" in {
    t.fields("double")    should be (Field("double",     ScalaTypes.ScalaDouble,     isRequired = true))
    t.fields("optDouble") should be (Field("optDouble",  ScalaTypes.ScalaDouble,     isRequired = false))
  }

  "BigInt -> Number" in {
    t.fields("integer")    should be (Field("integer",    ScalaTypes.ScalaBigInteger, isRequired = true))
    t.fields("optInteger") should be (Field("optInteger", ScalaTypes.ScalaBigInteger, isRequired = false))
  }

  "BigDecimal -> Number" in {
    t.fields("decimal")    should be (Field("decimal",    ScalaTypes.ScalaBigDecimal, isRequired = true))
    t.fields("optDecimal") should be (Field("optDecimal", ScalaTypes.ScalaBigDecimal, isRequired = false))
  }

  "sql.Date -> Date" in {
    t.fields("date")         should be (Field("date",         Plain.Date,       isRequired = true))
    t.fields("optDate")      should be (Field("optDate",      Plain.Date,       isRequired = false))
  }

  "sql.Time -> Time" in {
    t.fields("time")         should be (Field("time",         Plain.Time,       isRequired = true))
    t.fields("optTime")      should be (Field("optTime",      Plain.Time,       isRequired = false))
  }

  "sql.Timestamp -> Timestamp" in {
    t.fields("timestamp")    should be (Field("timestamp",    Plain.Timestamp,  isRequired = true))
    t.fields("optTimestamp") should be (Field("optTimestamp", Plain.Timestamp,  isRequired = false))
  }

  "utils.UUID -> UUID" in {
    t.fields("uuid")    should be (Field("uuid",     Plain.UUID, isRequired = true))
    t.fields("optUuid") should be (Field("optUuid",  Plain.UUID, isRequired = false))
  }
}
