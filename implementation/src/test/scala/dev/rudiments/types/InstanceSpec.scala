package dev.rudiments.types

import dev.rudiments.types.hard.ScalaTypes
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable.ListMap

@RunWith(classOf[JUnitRunner])
class InstanceSpec extends WordSpec with Matchers {
  private implicit val typeSystem: TypeSystem = TypeSystem()
  typeSystem.asThing[Thing]

  "instance is a part of TypeSystem" in {
    typeSystem.getType("Instance") should be (
      Type(
        "Instance",
        ListMap(
          "fields" -> Field(Index(ScalaTypes.ScalaString, Type("Anything", Map.empty)), true) //TODO put Anything to typeSystem
        )
      )
    )
  }

  "can build Instance of basic class" in {
    val t: Type = typeSystem.asType[C]
    t.construct("some string") should be (Instance(Map("s" -> "some string"))(t))
  }

  "can build Instance of algebraic class" in {
    val t: Type = typeSystem.asType[AlgebraicExample]

    val b = typeSystem.getOnlyOne("B")
    t.construct(b) should be (Instance(Map("a" -> b))(t))

    val c = typeSystem.getType("C").construct("another string")
    t.construct(c) should be (Instance(Map("a" -> c))(t))
  }

  case class SomeType(f: Boolean) extends DTO

  "can build Instance of a Type" in {
    val t: Type = typeSystem.getType("Type")

    t.fromScala(typeSystem.asType[SomeType]) should be (
      Instance(
        Map(
          "name" -> "SomeType",
          "fields" -> Map("f" -> Instance(
            Map(
              "type" -> Plain.Bool,
              "isRequired" -> true,
              "default" -> Instance(Map("fields" -> Map.empty))(typeSystem.getType("Instance"))
            )
          )(typeSystem.getType("Field"))),
          "ascendants" -> Seq.empty
        )
      )(t)
    )
  }

  sealed trait SomeTrait extends ADT
  abstract class SomeAbstract extends SomeTrait
  case object SomeObject extends SomeTrait
  case class SomeAnotherType(a: String) extends SomeTrait

  typeSystem.asThing[SomeTrait]

  "can build Instance of an Abstract" in {
    val t: Type = typeSystem.getType("Abstract")

    t.fromScala(typeSystem.getAbstract("SomeAbstract")) should be (
      Instance(
        Map(
          "name" -> "SomeAbstract",
          "ascendants" -> Seq(typeSystem.getAbstract("SomeTrait"))
        )
      )(t)
    )
  }

  "can build Instance of an OnlyOne" in {
    val t: Type = typeSystem.getType("OnlyOne")

    t.fromScala(typeSystem.getOnlyOne("SomeObject")) should be (
      Instance(
        Map(
          "name" -> "SomeObject",
          "ascendants" -> Seq(typeSystem.getAbstract("SomeTrait")),
          "value" -> Instance(Map("fields" -> Map.empty))(typeSystem.getType("Instance"))
        )
      )(t)
    )
  }

  "can build Instance of Algebraic" in {
    val t: Type = typeSystem.getType("Algebraic")

    val asc = Seq(typeSystem.getAbstract("SomeTrait"))

    t.fromScala(typeSystem.getAlgebraic("SomeTrait")) should be (
      Instance(
        Map(
          "root" -> Instance(Map("name" -> "SomeTrait", "ascendants" -> Seq.empty))(typeSystem.getType("Abstract")),
          "descendants" -> Set(
            OnlyOne("SomeObject", asc, Empty),
            Abstract("SomeAbstract", asc),
            Type("SomeAnotherType", Map("a" -> Field(ScalaTypes.ScalaString, true)), asc)
          ),
          "ascendants" -> Seq.empty
        )
      )(t)
    )
  }
}
