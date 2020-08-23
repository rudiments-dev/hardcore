package dev.rudiments.another

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

  case class SomethingA(a: A) extends DTO
  case class SomethingF(f: F) extends DTO

  private implicit val typeSystem: TypeSystem = TypeSystem()

  "can save cross-dependent ADT into type system" in {
    val t = typeSystem.makeFromScala[Spec, SomethingA]
    t.name should be ("SomethingA")

    val f = typeSystem.makeFromScala[Spec, SomethingF]
    f.name should be ("SomethingF")

    typeSystem.find[Abstract]("A") should be (Abstract("A"))
    typeSystem.find[The]("B") should be (The("B"))
    typeSystem.find[Spec]("C") should be (Spec("C", ListMap("f" -> ValueSpec(typeSystem.find[Abstract]("F"), true))))

    typeSystem.find[Abstract]("F") should be (Abstract("F"))
    typeSystem.find[Spec]("E") should be (Spec("E", ListMap("s" -> ValueSpec(ScalaTypes.ScalaString, true))))
    typeSystem.find[Spec]("D") should be (
      Spec("D", ListMap("a" -> ValueSpec(typeSystem.find[Abstract]("A"), true)))
    )
  }

  "can save all Plain in TypeSystem" in {
    typeSystem.makeFromScala[Thing, Plain] // with all Plain descendants

    typeSystem.find[Abstract]("Plain") should be (Abstract("Plain"))

    typeSystem.find[The]("Bool") should be       (The("Bool"))
    typeSystem.find[The]("Date") should be       (The("Date"))
    typeSystem.find[The]("Time") should be       (The("Time"))
    typeSystem.find[The]("Timestamp") should be  (The("Timestamp"))
    typeSystem.find[The]("UUID") should be       (The("UUID"))

    val nf = Seq(Abstract("NumberFormat"))
    typeSystem.children("NumberFormat") should be (
      Set(
        The("Integer"),
        The("Float"),
        The("Decimal"),
      )
    )

    typeSystem.children("Size") should be (
      Set(
        Spec("Big", ListMap(
          "size"-> ValueSpec(Plain.Number(Size.NegativeInfinity, Size.PositiveInfinity, NumberFormat.Decimal), true)
        )),
        The("Infinity"),
        The("PositiveInfinity"),
        The("NegativeInfinity"),
      )
    )

    typeSystem.find[Spec]("Text") should be (
      Spec("Text", ListMap("maxSize" -> ValueSpec(typeSystem.find[Abstract]("Size"), true)))
    )

    typeSystem.find[Spec]("Number") should be (
      Spec("Number", ListMap(
        "min" -> ValueSpec(typeSystem.find[Abstract]("Size"), true),
        "max" -> ValueSpec(typeSystem.find[Abstract]("Size"), true),
        "format" -> ValueSpec(typeSystem.find[Abstract]("NumberFormat"), true)
      ))
    )
  }

  "can save Thing in TypeSystem" in {
    typeSystem.makeFromScala[Thing, Thing]
    typeSystem.find[Abstract]("Thing") should be (Abstract("Thing"))
  }

  "can save Abstract in TypeSystem" in {
    typeSystem.find[Spec]("Abstract") should be (
      Spec(
        "Abstract",
        ListMap(
          "name" -> ValueSpec(ScalaTypes.ScalaString, true)
        )
      )
    )
  }

  "can save OnlyOne in TypeSystem" in {
    typeSystem.find[Spec]("The") should be (
      Spec(
        "The",
        ListMap(
          "name" -> ValueSpec(ScalaTypes.ScalaString, true),
//          "value" -> ValueSpec(typeSystem.find[Spec]("Instance"), false)
        )
      )
    )
  }

  "can save Type in TypeSystem" in {
    typeSystem.find[Spec]("Spec") should be (
      Spec(
        "Spec",
        ListMap(
          "name" -> ValueSpec(ScalaTypes.ScalaString, true),
          "fields" -> ValueSpec(Index(ScalaTypes.ScalaString, typeSystem.find[Spec]("ValueSpec")), true)
        )
      )
    )
  }

  "can save Field in TypeSystem" in {
    typeSystem.find[Spec]("ValueSpec") should be (
      Spec(
        "ValueSpec",
        ListMap(
          "thing" -> ValueSpec(typeSystem.find[Abstract]("Thing"), true),
          "isRequired" -> ValueSpec(Plain.Bool, true)
        )
      )
    )
  }

  "can save Instance in TypeSystem" in {
    typeSystem.makeFromScala[Thing, Instance]
    typeSystem.find[Spec]("Instance") should be (
      Spec(
        "Instance",
        ListMap(
          "spec" -> ValueSpec(typeSystem.find[Spec]("Spec"), true),
          "values" -> ValueSpec(List(The("Anything")), true) //TODO put Anything to typeSystem
        )
      )
    )
  }

  "can save List and Index in TypeSystem" in {
    typeSystem.find[Spec]("List") should be (
      Spec(
        "List",
        ListMap(
          "of" -> ValueSpec(typeSystem.find[Abstract]("Thing"), true) //TODO put Anything to typeSystem
        )
      )
    )

    typeSystem.find[Spec]("Index") should be (
      Spec(
        "Index",
        ListMap(
          "of" -> ValueSpec(typeSystem.find[Abstract]("Thing"), true), //TODO put Anything to typeSystem
          "over" -> ValueSpec(typeSystem.find[Abstract]("Thing"), true) //TODO put Anything to typeSystem
        )
      )
    )
  }
}