package dev.rudiments.hardcore.types

import org.junit.runner.RunWith
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.junit.JUnitRunner

import scala.collection.immutable.ListMap

@RunWith(classOf[JUnitRunner])
class TypeSpec extends WordSpec with Matchers {
  val t: ScalaType[Sample1] = ScalaType[Sample1]
  val tt: ScalaType[Type] = ScalaType[Type]

  "can construct soft instance" in {
    val s = t.construct(1, None, Set.empty)
    s should be (SoftInstance(Map("a" -> 1, "b" -> None, "c" -> Set.empty))(t))
    s.fields.head should be ("a" -> 1)
    s.fields.last should be ("c" -> Set.empty)
  }

  "fail if invalid argument" in {
    assertThrows[SoftValidationError] {
      val i = SoftInstance("1", Some("thing"), Set.empty)(t)
      t.extract(i, "a") // without extraction - does not fail
    }
  }

  "can extract value from soft instance" in {
    val s = t.construct(1, None, Set.empty)
    t.extract(s, "a") should be (1)
    t.extract(s, "b") should be (None)
    t.extract(s, "c") should be (Set.empty)
  }

  "can construct instance of HardType as T" in {
    t.constructScala(1, None, Set.empty) should be (Sample1(1, None, Set.empty))
  }

  "can extract value from T with HardType by field name" in {
    val v = t.constructScala(1, None, Set.empty)
    t.extract(v, "a") should be (1)
    t.extract(v, "b") should be (None)
    t.extract(v, "c") should be (Set.empty)
  }

  "can construct type of type" in {
    val m = tt.constructScala("FirstSyntheticType", ListMap(
      "firstSyntheticField" -> Field(ScalaTypes.ScalaLong, FieldFlag.Required),
      "secondSyntheticField" -> Field(ScalaTypes.ScalaString, FieldFlag.Optional)
    ))
    m should be (ScalaType[FirstSyntheticType])
  }

  "synthetic type should be able to construct a Map with values of Type" in {
    val m = tt.constructScala("FirstSyntheticType", ListMap(
      "firstSyntheticField" -> Field(ScalaTypes.ScalaLong, FieldFlag.Required),
      "secondSyntheticField" -> Field(ScalaTypes.ScalaString, FieldFlag.Optional)
    ))
    m.construct(42L, Some("Because")) should be (SoftInstance(Map(
      "firstSyntheticField" -> 42L,
      "secondSyntheticField" -> Some("Because")
    ))(tt))
  }

  case class FirstSyntheticType (
    firstSyntheticField: Long,
    secondSyntheticField: Option[String]
  ) extends DTO
}