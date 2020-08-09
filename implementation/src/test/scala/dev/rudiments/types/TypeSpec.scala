package dev.rudiments.types

import dev.rudiments.types.NumberSize.Big
import dev.rudiments.types.hard.{ScalaType, ScalaTypes}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable.ListMap

@RunWith(classOf[JUnitRunner])
class TypeSpec extends WordSpec with Matchers {
  private implicit val typeSystem: TypeSystem = TypeSystem()
  val t: Type = typeSystem.asType[Sample1]

  "can construct soft instance" in {
    val s = t.construct(1, None, Set.empty)
    s should be (Instance(Map("a" -> 1, "b" -> None, "c" -> Set.empty))(t))
    s.fields.head should be ("a" -> 1)
    s.fields.last should be ("c" -> Set.empty)
  }

  "fail if invalid argument" in {
    assertThrows[SoftValidationError] {
      val i = Instance("1", Some("thing"), Set.empty)(t)
      i.extract("a") // without extraction - does not fail
    }
  }

  "can extract value from soft instance" in {
    val s = t.construct(1, None, Set.empty)
    s.extract[Any]("a") should be (1)
    s.extract[Any]("b") should be (None)
    s.extract[Any]("c") should be (Set.empty)
  }

  val tt: ScalaType[Type] = typeSystem.asType[Type]
  "type of type contains ADT" in {
    tt.name should be ("Type")
    tt.fields("fields").`type` should be (Index(Plain.Text(ScalaTypes.MaxInt), typeSystem.asType[Field]))
    //Field contains Thing: ADT
  }

  "can construct type of type" in {
    val m = tt.constructScala("FirstSyntheticType", ListMap(
      "firstSyntheticField" -> Field("firstSyntheticField", ScalaTypes.ScalaLong, isRequired = true),
      "secondSyntheticField" -> Field("secondSyntheticField", ScalaTypes.ScalaString, isRequired = false)
    ), Seq.empty[Thing])
    m should be (typeSystem.asType[FirstSyntheticType])
  }

  "synthetic type should be able to construct Instance with values of Type" in {
    val m = tt.constructScala("FirstSyntheticType", ListMap(
      "firstSyntheticField" -> Field("firstSyntheticField", ScalaTypes.ScalaLong, isRequired = true),
      "secondSyntheticField" -> Field("secondSyntheticField", ScalaTypes.ScalaString, isRequired = false)
    ), Seq.empty[Thing])
    m.construct(42L, Some("Because")) should be (Instance(Map(
      "firstSyntheticField" -> 42L,
      "secondSyntheticField" -> Some("Because")
    ))(m))
  }

  case class FirstSyntheticType (
    firstSyntheticField: Long,
    secondSyntheticField: Option[String]
  ) extends DTO
}