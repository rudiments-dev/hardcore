package dev.rudiments.types

import dev.rudiments.types.hard.ScalaTypes
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable.ListMap

@RunWith(classOf[JUnitRunner])
class TypeSystemSpec extends WordSpec with Matchers {

  sealed trait A extends ADT {
    case object B extends A
    case class C(f: F) extends A
  }

  sealed trait F extends ADT {
    case class E(s: String) extends F
    case class D(a: A) extends F
  }

  case class SomethingA(a: A)
  case class SomethingF(f: F)

  private implicit val typeSystem: TypeSystem = TypeSystem()

  "can save cross-dependent ADT into type system" in {
    val t = typeSystem.asType[SomethingA]
    t.name should be ("SomethingA")

    val f = typeSystem.asType[SomethingF]
    f.name should be ("SomethingF")

    typeSystem.getAbstract("A") should be (Abstract("A"))
    typeSystem.getOnlyOne("B") should be (OnlyOne("B", Seq(Abstract("A"))))
    typeSystem.getType("C") should be (
      Type("C", Map("f" -> ValueSpec(typeSystem.getAlgebraic("F"), true)), Seq(Abstract("A")))
    )

    typeSystem.getAbstract("F") should be (Abstract("F"))
    typeSystem.getType("E") should be (
      Type("E", ListMap("s" -> ValueSpec(ScalaTypes.ScalaString, true)), Seq(Abstract("F")))
    )
    typeSystem.getType("D") should be (
      Type("D", ListMap("a" -> ValueSpec(typeSystem.getAbstract("A"), true)), Seq(Abstract("F")))
//TODO fix abstract -> algebraic in type fields      Type("D", ListMap("a" -> Field(typeSystem.getAlgebraic("A"), true)), Seq(Abstract("F")))
    )
  }

  "can save all Plain in TypeSystem" in {
    typeSystem.asThing[Plain] // with all Plain descendants

    typeSystem.getAbstract("Plain") should be (Abstract("Plain"))

    typeSystem.getOnlyOne("Bool") should be       (OnlyOne("Bool",      Seq(Abstract("Plain"))))
    typeSystem.getOnlyOne("Date") should be       (OnlyOne("Date",      Seq(Abstract("Plain"))))
    typeSystem.getOnlyOne("Time") should be       (OnlyOne("Time",      Seq(Abstract("Plain"))))
    typeSystem.getOnlyOne("Timestamp") should be  (OnlyOne("Timestamp", Seq(Abstract("Plain"))))
    typeSystem.getOnlyOne("UUID") should be       (OnlyOne("UUID",      Seq(Abstract("Plain"))))

    val nf = Seq(Abstract("NumberFormat"))
    typeSystem.getAlgebraic("NumberFormat") should be (
      Algebraic(Abstract("NumberFormat"), Set(
        OnlyOne("Integer", nf),
        OnlyOne("Float", nf),
        OnlyOne("Decimal", nf),
      ))
    )

    val ns = Seq(Abstract("NumberSize"))
    typeSystem.getAlgebraic("NumberSize") should be (
      Algebraic(Abstract("NumberSize"), Set(
        Type("Big", ListMap("size"-> ValueSpec(Plain.Number(NumberSize.NegativeInfinity,NumberSize.PositiveInfinity,NumberFormat.Decimal), true)), ns),
        OnlyOne("Infinity", ns),
        OnlyOne("PositiveInfinity", ns),
        OnlyOne("NegativeInfinity", ns),
      ))
    )

    typeSystem.getType("Text") should be (
      Type("Text", ListMap("maxSize" -> ValueSpec(typeSystem.getAlgebraic("NumberSize"), true)), Seq(Abstract("Plain")))
    )

    typeSystem.getType("Number") should be (
      Type("Number", ListMap(
        "min" -> ValueSpec(typeSystem.getAlgebraic("NumberSize"), true),
        "max" -> ValueSpec(typeSystem.getAlgebraic("NumberSize"), true),
        "format" -> ValueSpec(typeSystem.getAlgebraic("NumberFormat"), true)
      ), Seq(Abstract("Plain")))
    )
  }

  "can save Thing in TypeSystem" in {
    typeSystem.asThing[Thing]
    typeSystem.getAbstract("Thing") should be (Abstract("Thing"))
  }

  "can save Abstract in TypeSystem" in {
    typeSystem.getType("Abstract") should be (
      Type(
        "Abstract",
        ListMap(
          "name" -> ValueSpec(ScalaTypes.ScalaString, true),
          "ascendants" -> ValueSpec(List(typeSystem.getAbstract("Thing")), false) //TODO Abstract -> Algebraic
        ),
        Seq(Abstract("Thing"))
      )
    )
  }

  "can save OnlyOne in TypeSystem" in {
    typeSystem.getType("OnlyOne") should be (
      Type(
        "OnlyOne",
        ListMap(
          "name" -> ValueSpec(ScalaTypes.ScalaString, true),
          "ascendants" -> ValueSpec(List(typeSystem.getAbstract("Thing")), false), //TODO Abstract -> Algebraic
          "value" -> ValueSpec(typeSystem.getType("Instance"), false)
        ),
        Seq(Abstract("Thing"))
      )
    )
  }

  "can save Type in TypeSystem" in {
    typeSystem.getType("Type") should be (
      Type(
        "Type",
        ListMap(
          "name" -> ValueSpec(ScalaTypes.ScalaString, true),
          "fields" -> ValueSpec(Index(ScalaTypes.ScalaString, typeSystem.getType("ValueSpec")), true),
          "ascendants" -> ValueSpec(List(typeSystem.getAbstract("Thing")), false) //TODO Abstract -> Algebraic
        ),
        Seq(Abstract("Thing"))
      )
    )
  }

  "can save Field in TypeSystem" in {
    typeSystem.getType("ValueSpec") should be (
      Type(
        "ValueSpec",
        ListMap(
          "type" -> ValueSpec(typeSystem.getAbstract("Thing"), true), //TODO Abstract -> Algebraic
          "isRequired" -> ValueSpec(Plain.Bool, true),
          "default" -> ValueSpec(typeSystem.getType("Instance"), false)
        )
      )
    )
  }

  "can save Instance in TypeSystem" in {
    typeSystem.getType("Instance") should be (
      Type(
        "Instance",
        ListMap(
          "fields" -> ValueSpec(Index(ScalaTypes.ScalaString, Type("Anything", Map.empty)), true) //TODO put Anything to typeSystem
        )
      )
    )
  }

  "can save List and Index in TypeSystem" in {
    typeSystem.getType("List") should be (
      Type(
        "List",
        ListMap(
          "of" -> ValueSpec(typeSystem.getAbstract("Thing"), true) //TODO put Anything to typeSystem
        ),
        Seq(Abstract("Thing"))
      )
    )

    typeSystem.getType("Index") should be (
      Type(
        "Index",
        ListMap(
          "of" -> ValueSpec(typeSystem.getAbstract("Thing"), true), //TODO put Anything to typeSystem
          "over" -> ValueSpec(typeSystem.getAbstract("Thing"), true) //TODO put Anything to typeSystem
        ),
        Seq(Abstract("Thing"))
      )
    )
  }
}