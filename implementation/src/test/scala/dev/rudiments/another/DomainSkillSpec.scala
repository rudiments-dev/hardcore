package dev.rudiments.another

import dev.rudiments.another.CRUD._
import dev.rudiments.another.ReadOnly._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable.ListMap


@RunWith(classOf[JUnitRunner])
class DomainSkillSpec extends WordSpec with Matchers {
  private val domain = new DomainSkill()

  "initial content of domain" in {
    domain(Count()).merge should be (Counted(26))
  }

  "can create Abstract" in {
    domain(Create(ID(Seq("SampleAbstract")), Instance(domain.abs, Seq("SampleAbstract")))).merge should be (
      Created(ID(Seq("SampleAbstract")), Instance(domain.abs, Seq("SampleAbstract")))
    )

    domain(Find(ID(Seq("SampleAbstract")))).merge should be (
      Found(ID(Seq("SampleAbstract")), Instance(domain.abs, Seq("SampleAbstract")))
    )
  }

  "can create The" in {
    domain(Create(ID(Seq("SampleSingleton")), Instance(domain.the, Seq("SampleSingleton")))).merge should be (
      Created(ID(Seq("SampleSingleton")), Instance(domain.the, Seq("SampleSingleton")))
    )

    domain(Find(ID(Seq("SampleSingleton")))).merge should be (
      Found(ID(Seq("SampleSingleton")), Instance(domain.the, Seq("SampleSingleton")))
    )
  }

  "can create Spec" in {
    domain(
      Create(
        ID(Seq("SampleSpec")),
        Instance(domain.spec, Seq(
          "SampleSpec",
          "dev.rudiments.DomainSkillSpec.SampleSpec",
          ListMap(
            "a" -> Instance(domain.valueSpec, Seq(The("Bool"), true))
          )
        ))
      )
    ).merge should be (
      Created(
        ID(Seq("SampleSpec")),
        Instance(domain.spec, Seq(
          "SampleSpec",
          "dev.rudiments.DomainSkillSpec.SampleSpec",
          ListMap(
            "a" -> Instance(domain.valueSpec, Seq(domain.the.fromProduct(domain.domain, The("Bool")), true))
          )
        ))
      )
    )

    domain(Find(ID(Seq("SampleSpec")))).merge should be (
      Found(
        ID(Seq("SampleSpec")),
        Instance(domain.spec, Seq(
          "SampleSpec",
          "dev.rudiments.DomainSkillSpec.SampleSpec",
          ListMap(
            "a" -> Instance(domain.valueSpec, Seq(domain.the.fromProduct(domain.domain, The("Bool")), true))
          )
        ))
      )
    )
  }

  "update thing" in {
    domain(Update(
      ID(Seq("SampleSingleton")),
      Instance(domain.abs, Seq("SampleSingleton"))
    )).merge should be (Updated(
      ID(Seq("SampleSingleton")),
      Instance(domain.the, Seq("SampleSingleton")),
      Instance(domain.abs, Seq("SampleSingleton"))
    ))

    domain(Find(ID(Seq("SampleSingleton")))).merge should be (
      Found(ID(Seq("SampleSingleton")), Instance(domain.abs, Seq("SampleSingleton")))
    )
  }

  "multiple inserts makes conflict" in {
    domain(Create(ID(Seq("SampleSingleton")), Instance(domain.the, Seq("SampleSingleton")))).merge should be (
      AlreadyExists(ID(Seq("SampleSingleton")), Instance(domain.abs, Seq("SampleSingleton")))
    )
  }

  "delete thing" in {
    domain(Delete(ID(Seq("SampleSingleton")))).merge should be (
      Deleted(ID(Seq("SampleSingleton")), Instance(domain.abs, Seq("SampleSingleton")))
    )
  }
}
