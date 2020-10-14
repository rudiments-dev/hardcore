package dev.rudiments.domain.registry

import dev.rudiments.data.CRUD._
import dev.rudiments.data.ReadOnly._
import dev.rudiments.domain.{ID, Instance, The}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable.ListMap


@RunWith(classOf[JUnitRunner])
class DomainSkillSpec extends WordSpec with Matchers {
  private val ctx = new DomainContext
  private val domain = new DomainSkill(ctx)

  "initial content of domain" in {
    domain(Count()) should be (Counted(26))
  }

  "can create Abstract" in {
    domain(Create(ID(Seq("SampleAbstract")), Instance(domain.ctx.abs, Seq("SampleAbstract", ListMap.empty)))) should be (
      Created(ID(Seq("SampleAbstract")), Instance(domain.ctx.abs, Seq("SampleAbstract", ListMap.empty)))
    )

    domain(Find(ID(Seq("SampleAbstract")))) should be (
      Found(ID(Seq("SampleAbstract")), Instance(domain.ctx.abs, Seq("SampleAbstract", ListMap.empty)))
    )
  }

  "can create The" in {
    domain(Create(ID(Seq("SampleSingleton")), Instance(domain.ctx.the, Seq("SampleSingleton")))) should be (
      Created(ID(Seq("SampleSingleton")), Instance(domain.ctx.the, Seq("SampleSingleton")))
    )

    domain(Find(ID(Seq("SampleSingleton")))) should be (
      Found(ID(Seq("SampleSingleton")), Instance(domain.ctx.the, Seq("SampleSingleton")))
    )
  }

  "can create Spec" in {
    domain(
      Create(
        ID(Seq("SampleSpec")),
        Instance(domain.ctx.spec, Seq(
          "SampleSpec",
          "dev.rudiments.DomainSkillSpec.SampleSpec",
          ListMap(
            "a" -> Instance(domain.ctx.valueSpec, Seq(The("Bool"), true))
          )
        ))
      )
    ) should be (
      Created(
        ID(Seq("SampleSpec")),
        Instance(domain.ctx.spec, Seq(
          "SampleSpec",
          "dev.rudiments.DomainSkillSpec.SampleSpec",
          ListMap(
            "a" -> Instance(domain.ctx.valueSpec, Seq(domain.ctx.the.fromProduct(domain.ctx.domain, The("Bool")), true))
          )
        ))
      )
    )

    domain(Find(ID(Seq("SampleSpec")))) should be (
      Found(
        ID(Seq("SampleSpec")),
        Instance(domain.ctx.spec, Seq(
          "SampleSpec",
          "dev.rudiments.DomainSkillSpec.SampleSpec",
          ListMap(
            "a" -> Instance(domain.ctx.valueSpec, Seq(domain.ctx.the.fromProduct(domain.ctx.domain, The("Bool")), true))
          )
        ))
      )
    )
  }

  "update thing" in {
    domain(Update(
      ID(Seq("SampleSingleton")),
      Instance(domain.ctx.abs, Seq("SampleSingleton", ListMap.empty))
    )) should be (Updated(
      ID(Seq("SampleSingleton")),
      Instance(domain.ctx.the, Seq("SampleSingleton")),
      Instance(domain.ctx.abs, Seq("SampleSingleton", ListMap.empty))
    ))

    domain(Find(ID(Seq("SampleSingleton")))) should be (
      Found(ID(Seq("SampleSingleton")), Instance(domain.ctx.abs, Seq("SampleSingleton", ListMap.empty)))
    )
  }

  "multiple inserts makes conflict" in {
    domain(Create(ID(Seq("SampleSingleton")), Instance(domain.ctx.the, Seq("SampleSingleton")))) should be (
      AlreadyExists(ID(Seq("SampleSingleton")), Instance(domain.ctx.abs, Seq("SampleSingleton", ListMap.empty)))
    )
  }

  "delete thing" in {
    domain(Delete(ID(Seq("SampleSingleton")))) should be (
      Deleted(ID(Seq("SampleSingleton")), Instance(domain.ctx.abs, Seq("SampleSingleton", ListMap.empty)))
    )
  }
}
