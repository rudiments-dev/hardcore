package dev.rudiments.hardcore.types

import org.junit.runner.RunWith
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.junit.JUnitRunner

import scala.collection.immutable.ListMap

@RunWith(classOf[JUnitRunner])
class TypeSpec extends WordSpec with Matchers {
  val t: HardType[Sample1] = HardType[Sample1]
  val tt: HardType[Type] = HardType[Type]

  "can construct map with values" in {
    val m = t.constructMap(1, None, Set.empty)
    m should be (Map("a" -> 1, "b" -> None, "c" -> Set.empty))
    m.head should be ("a" -> 1)
    m.last should be ("c" -> Set.empty)
  }

  "can construct instance of HardType as T" in {
    t.construct(1, None, Set.empty) should be (Sample1(1, None, Set.empty))
  }

  "can construct type of type" in {
    val m = tt.construct("FirstSyntheticType", ListMap(
      "firstSyntheticField" -> Field(RudimentTypes.Number, FieldFlags.Required),
      "secondSyntheticField" -> Field(RudimentTypes.Text, FieldFlags.Optional)
    ))
    m should be (Type[FirstSyntheticType])
  }

  "synthetic type should be able to construct a Map with values of Type" in {
    val m = tt.construct("FirstSyntheticType", ListMap(
      "firstSyntheticField" -> Field(RudimentTypes.Number, FieldFlags.Required),
      "secondSyntheticField" -> Field(RudimentTypes.Text, FieldFlags.Optional)
    ))
    m.constructMap(42L, Some("Because")) should be (Map(
      "firstSyntheticField" -> 42L,
      "secondSyntheticField" -> Some("Because")
    ))
  }

  case class FirstSyntheticType (
    firstSyntheticField: Long,
    secondSyntheticField: Option[String]
  ) extends DTO
}