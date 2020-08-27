package dev.rudiments.another

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable.ListMap

@RunWith(classOf[JUnitRunner])
class DomainSpec extends WordSpec with Matchers {

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

  private implicit val domain: Domain = Domain()

  "can save cross-dependent ADT into type system" in {
    val t = domain.makeFromScala[Spec, SomethingA]
    t.name should be ("SomethingA")

    val f = domain.makeFromScala[Spec, SomethingF]
    f.name should be ("SomethingF")

    domain.find[Abstract]("A") should be (Abstract("A"))
    domain.find[The]("B") should be (The("B"))
    domain.find[Spec]("C") should be (Spec("C", ListMap("f" -> ValueSpec(domain.find[Abstract]("F"), true))))

    domain.find[Abstract]("F") should be (Abstract("F"))
    domain.find[Spec]("E") should be (Spec("E", ListMap("s" -> ValueSpec(ScalaTypes.ScalaString, true))))
    domain.find[Spec]("D") should be (
      Spec("D", ListMap("a" -> ValueSpec(domain.find[Abstract]("A"), true)))
    )
  }

  "can save all Plain in Domain" in {
    domain.makeFromScala[Thing, Plain] // with all Plain descendants

    domain.find[Abstract]("Plain") should be (Abstract("Plain"))

    domain.find[The]("Bool") should be       (The("Bool"))
    domain.find[The]("Date") should be       (The("Date"))
    domain.find[The]("Time") should be       (The("Time"))
    domain.find[The]("Timestamp") should be  (The("Timestamp"))
    domain.find[The]("UUID") should be       (The("UUID"))

    domain.children("NumberFormat") should be (
      Set(
        The("Integer"),
        The("Float"),
        The("Decimal"),
      )
    )

    domain.children("Size") should be (
      Set(
        Spec("Big", ListMap(
          "size"-> ValueSpec(Plain.Number(Size.NegativeInfinity, Size.PositiveInfinity, NumberFormat.Decimal), true)
        )),
        The("Infinity"),
        The("PositiveInfinity"),
        The("NegativeInfinity"),
      )
    )

    domain.find[Spec]("Text") should be (
      Spec("Text", ListMap("maxSize" -> ValueSpec(domain.find[Abstract]("Size"), true)))
    )

    domain.find[Spec]("Number") should be (
      Spec("Number", ListMap(
        "min" -> ValueSpec(domain.find[Abstract]("Size"), true),
        "max" -> ValueSpec(domain.find[Abstract]("Size"), true),
        "format" -> ValueSpec(domain.find[Abstract]("NumberFormat"), true)
      ))
    )
  }

  "can save Thing in Domain" in {
    domain.makeFromScala[Thing, Thing]
    domain.find[Abstract]("Thing") should be (Abstract("Thing"))
  }

  "can save Abstract in Domain" in {
    domain.find[Spec]("Abstract") should be (
      Spec(
        "Abstract",
        ListMap(
          "name" -> ValueSpec(ScalaTypes.ScalaString, true)
        )
      )
    )
  }

  "can save OnlyOne in Domain" in {
    domain.find[Spec]("The") should be (
      Spec(
        "The",
        ListMap(
          "name" -> ValueSpec(ScalaTypes.ScalaString, true),
//          "value" -> ValueSpec(typeSystem.find[Spec]("Instance"), false)
        )
      )
    )
  }

  "can save Type in Domain" in {
    domain.find[Spec]("Spec") should be (
      Spec(
        "Spec",
        ListMap(
          "name" -> ValueSpec(ScalaTypes.ScalaString, true),
          "fields" -> ValueSpec(Index(ScalaTypes.ScalaString, domain.find[Spec]("ValueSpec")), true)
        )
      )
    )
  }

  "can save Field in Domain" in {
    domain.find[Spec]("ValueSpec") should be (
      Spec(
        "ValueSpec",
        ListMap(
          "thing" -> ValueSpec(domain.find[Abstract]("Thing"), true),
          "isRequired" -> ValueSpec(Plain.Bool, true)
        )
      )
    )
  }

  "can save Instance in Domain" in {
    domain.makeFromScala[Thing, Instance]
    domain.find[Spec]("Instance") should be (
      Spec(
        "Instance",
        ListMap(
          "spec" -> ValueSpec(domain.find[Spec]("Spec"), true),
          "values" -> ValueSpec(List(The("Anything")), true) //TODO put Anything to Domain
        )
      )
    )
  }

  "can save List and Index in Domain" in {
    domain.find[Spec]("List") should be (
      Spec(
        "List",
        ListMap(
          "of" -> ValueSpec(domain.find[Abstract]("Thing"), true)
        )
      )
    )

    domain.find[Spec]("Index") should be (
      Spec(
        "Index",
        ListMap(
          "of" -> ValueSpec(domain.find[Abstract]("Thing"), true),
          "over" -> ValueSpec(domain.find[Abstract]("Thing"), true)
        )
      )
    )
  }
}