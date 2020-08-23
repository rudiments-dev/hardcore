package dev.rudiments.another

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable.ListMap

@RunWith(classOf[JUnitRunner])
class InstanceTest extends WordSpec with Matchers {
  private implicit val typeSystem: TypeSystem = TypeSystem()
  typeSystem.makeFromScala[Thing, Thing]
  typeSystem.makeFromScala[Thing, Instance]

  "Instance is a part of TypeSystem" in {
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

  "can build Instance of basic class" in {
    typeSystem.makeFromScala[Spec, AlgebraicExample]
    val t: Spec = typeSystem.find[Spec]("C")
    t.instantiate(typeSystem, "some string") should be (
      Instance(t, Seq("some string"))
    )
  }

  "can build Instance of algebraic class" in {
    val t: Spec = typeSystem.find[Spec]("AlgebraicExample")

    val b = typeSystem.find[The]("B")
    t.instantiate(typeSystem, b) should be (Instance(t, Seq(b)))

    val c = typeSystem.find[Spec]("C").instantiate(typeSystem, "another string")
    t.instantiate(typeSystem, c) should be (Instance(t, Seq(c)))
  }

  case class SomeType(f: Boolean) extends DTO

  "can build Instance of a Type" in {
    val t: Spec = typeSystem.find[Spec]("Spec")

    t.fromProduct(typeSystem, typeSystem.makeFromScala[Spec, SomeType]) should be (
      Instance(
        typeSystem.find[Spec]("Spec"),
        Seq(
          "SomeType",
          ListMap(
            "f" -> Instance(
              typeSystem.find[Spec]("ValueSpec"),
              Seq(Plain.Bool, true)
            )
          )
        )
      )
    )
  }

  sealed trait SomeTrait extends ADT
  abstract class SomeAbstract extends SomeTrait
  case object SomeObject extends SomeTrait
  case class SomeAnotherType(a: String) extends SomeTrait

  typeSystem.makeFromScala[Thing, SomeTrait]

  "can build Instance of an Abstract" in {
    val t: Spec = typeSystem.find[Spec]("Abstract")

    t.fromProduct(typeSystem, typeSystem.find[Abstract]("SomeAbstract")) should be (
      Instance(t, Seq("SomeAbstract"))
    )
  }

  "can build Instance of The" in {
    val t: Spec = typeSystem.find[Spec]("The")

    t.fromProduct(typeSystem, typeSystem.find[The]("SomeObject")) should be (
      Instance(t, Seq("SomeObject"))
    )
  }
}
