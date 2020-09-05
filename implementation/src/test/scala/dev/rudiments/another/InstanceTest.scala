package dev.rudiments.another

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable.ListMap

@RunWith(classOf[JUnitRunner])
class InstanceTest extends WordSpec with Matchers {
  private implicit val domain: Domain = Domain()
  domain.makeFromScala[Thing, Thing]
  domain.makeFromScala[Thing, Instance]

  "Instance is a part of TypeSystem" in {
    domain.find[Spec]("Instance") should be (
      Spec(
        "Instance",
        "dev.rudiments.another.Instance",
        ListMap(
          "spec" -> ValueSpec(domain.find[Spec]("Spec"), true),
          "values" -> ValueSpec(List(The("Anything")), true)
        )
      )
    )
  }

  "can build Instance of basic class" in {
    domain.makeFromScala[Spec, AlgebraicExample]
    val t: Spec = domain.find[Spec]("C")
    t.instantiate(domain, "some string") should be (
      Instance(t, Seq("some string"))
    )
  }

  "can build Instance of algebraic class" in {
    val t: Spec = domain.find[Spec]("AlgebraicExample")

    val b = domain.find[The]("B")
    t.instantiate(domain, b) should be (Instance(t, Seq(b)))

    val c = domain.find[Spec]("C").instantiate(domain, "another string")
    t.instantiate(domain, c) should be (Instance(t, Seq(c)))
  }

  case class SomeType(f: Boolean) extends DTO

  "can build Instance of a Type" in {
    val t: Spec = domain.find[Spec]("Spec")

    t.fromProduct(domain, domain.makeFromScala[Spec, SomeType]) should be (
      Instance(
        domain.find[Spec]("Spec"),
        Seq(
          "SomeType",
          "dev.rudiments.another.InstanceTest.SomeType",
          ListMap(
            "f" -> Instance(
              domain.find[Spec]("ValueSpec"),
              Seq(The("Bool"), true)
            )
          )
        )
      )
    )
  }

  "can build Instance of a Spec" in {
    val spec: Spec = domain.find[Spec]("Spec")
    val valueSpec = domain.find[Spec]("ValueSpec")

    val textInstance = Instance(
      domain.find[Spec]("Text"),
      Seq(Instance(
        domain.find[Spec]("Big"),
        Seq(BigDecimal(Int.MaxValue))
      ))
    )

    val valueSpecInstance = spec.fromProduct(domain, valueSpec)

    val indexInstance = Instance(
      domain.find[Spec]("Index"),
      Seq(textInstance, valueSpecInstance)
    )

    spec.fromProduct(domain, spec) should be (
      Instance(
        spec,
        Seq(
          "Spec",
          "dev.rudiments.another.Spec",
          ListMap(
            "name" -> Instance(
              valueSpec,
              Seq(textInstance, true)
            ),
            "fullName" -> Instance(
              valueSpec,
              Seq(textInstance, true)
            ),
            "fields" -> Instance(
              valueSpec,
              Seq(indexInstance, true)
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

  domain.makeFromScala[Thing, SomeTrait]

  "can build Instance of an Abstract" in {
    val t: Spec = domain.find[Spec]("Abstract")

    t.fromProduct(domain, domain.find[Abstract]("SomeAbstract")) should be (
      Instance(t, Seq("SomeAbstract"))
    )
  }

  "can build Instance of The" in {
    val t: Spec = domain.find[Spec]("The")

    t.fromProduct(domain, domain.find[The]("SomeObject")) should be (
      Instance(t, Seq("SomeObject"))
    )
  }
}
