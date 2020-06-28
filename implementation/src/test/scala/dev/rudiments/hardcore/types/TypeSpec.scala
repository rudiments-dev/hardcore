package dev.rudiments.hardcore.types

import dev.rudiments.hardcore.types.Types.Reference
import org.junit.runner.RunWith
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.junit.JUnitRunner

import scala.collection.immutable.ListMap

@RunWith(classOf[JUnitRunner])
class TypeSpec extends WordSpec with Matchers {
  private implicit val typeSystem: TypeSystem = new TypeSystem()
  val t: ScalaType[Sample1] = ScalaType[Sample1]

  "can construct soft instance" in {
    val s = t.construct(1, None, Set.empty).asInstanceOf[SoftInstance]
    s should be (SoftInstance(Map("a" -> 1, "b" -> None, "c" -> Set.empty))(t))
    s.fields.head should be ("a" -> 1)
    s.fields.last should be ("c" -> Set.empty)
  }

  "fail if invalid argument" in {
    assertThrows[SoftValidationError] {
      val i = SoftInstance("1", Some("thing"), Set.empty)(t)
      i.extract("a") // without extraction - does not fail
    }
  }

  "can extract value from soft instance" in {
    val s = t.construct(1, None, Set.empty)
    s.extract[Any]("a") should be (1)
    s.extract[Any]("b") should be (None)
    s.extract[Any]("c") should be (Set.empty)
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

  case class FieldFlagContainer(flag: FieldFlag) extends DTO

  "can construct FieldFlag enum" in {
    val tt = ScalaType[FieldFlagContainer]
    tt.name should be ("FieldFlagContainer")
    tt.fields("flag") should be (Field(Reference(Algebraic(
      "dev.rudiments.hardcore.types.FieldFlag",
      Declaration("FieldFlag"),
      Seq(
        Singleton("Required"),
        Singleton("Optional"),
        Singleton("WithDefault"),
        Singleton("NonEmpty"),
        Singleton("CanBeEmpty")
      )
    )), FieldFlag.Required))
  }

  val tt: ScalaType[Type] = ScalaType[Type]
  "type of type contains ADT" ignore {
    tt.name should be ("Type")
    tt.fields should be (ListMap(
      "name" -> Field(ScalaTypes.ScalaString, FieldFlag.Required),
      "fields" -> Field(
        Types.Index(
          ScalaTypes.ScalaString,
          Types.Reference(Type(
            "Field", ListMap(
              "kind" -> Field(Types.Unknown, FieldFlag.Required),
              "fieldFlag" -> Field(Types.Reference(new Enum(
                "dev.rudiments.hardcore.types.FieldFlag",
                Declaration("FieldFlag"),
                Seq(
                  Singleton("Required"),
                  Singleton("Optional"),
                  Singleton("WithDefault"),
                  Singleton("NonEmpty"),
                  Singleton("CanBeEmpty")
                )
              )), FieldFlag.Required)
            )
          ))
        ), FieldFlag.NonEmpty)
    ))
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