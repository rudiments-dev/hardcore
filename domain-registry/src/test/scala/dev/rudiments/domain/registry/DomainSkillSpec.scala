package dev.rudiments.domain.registry

import dev.rudiments.data._
import dev.rudiments.domain.{Domain, ID, Instance, The}
import dev.rudiments.hardcore.All
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable.ListMap


@RunWith(classOf[JUnitRunner])
class DomainSkillSpec extends WordSpec with Matchers {
  private val domain = new Domain

  "initial content of domain" in {
    domain(Count(All)) should be (Counted(27))
    val found = domain(FindAll(All))
    found.flatMap[FoundAll] { f =>
      f
    }
  }

  "can create Abstract" in {
    domain(
      Create(
        ID(Seq("SampleAbstract")),
        Instance(domain.t, Seq(
          "SampleAbstract",
          Instance(domain.abs, Seq("SampleAbstract", ListMap.empty)),
          Seq.empty
        ))
      )
    ) should be (
      Created(
        ID(Seq("SampleAbstract")),
        Instance(domain.t, Seq(
          "SampleAbstract",
          Instance(domain.abs, Seq("SampleAbstract", ListMap.empty)),
          Seq.empty
        ))
      )
    )

    domain(Find(ID(Seq("SampleAbstract")))) should be (
      Found(
        ID(Seq("SampleAbstract")),
        Instance(domain.t, Seq(
          "SampleAbstract",
          Instance(domain.abs, Seq("SampleAbstract", ListMap.empty)),
          Seq.empty
        ))
      )
    )
  }

  "can create The" in {
    domain(
      Create(
        ID(Seq("SampleSingleton")),
        Instance(domain.t, Seq(
          "SampleSingleton",
          Instance(domain.the, Seq("SampleSingleton")),
          Seq.empty
        ))
      )
    ) should be (
      Created(
        ID(Seq("SampleSingleton")),
        Instance(domain.t, Seq(
          "SampleSingleton",
          Instance(domain.the, Seq("SampleSingleton")),
          Seq.empty
        ))
      )
    )

    domain(Find(ID(Seq("SampleSingleton")))) should be (
      Found(
        ID(Seq("SampleSingleton")),
        Instance(domain.t, Seq(
          "SampleSingleton",
          Instance(domain.the, Seq("SampleSingleton")),
          Seq.empty
        ))
      )
    )
  }

  "can create Spec" in {
    domain(
      Create(
        ID(Seq("SampleSpec")),
        Instance(domain.t, Seq(
          "SampleSpec",
          Instance(domain.spec, Seq(
            "SampleSpec",
            "dev.rudiments.DomainSkillSpec.SampleSpec",
            ListMap(
              "a" -> Instance(domain.valueSpec, Seq(The("Bool"), true))
            )
          )),
          Seq.empty
        ))
      )
    ) should be (
      Created(
        ID(Seq("SampleSpec")),
        Instance(domain.t, Seq(
          "SampleSpec",
          Instance(domain.spec, Seq(
            "SampleSpec",
            "dev.rudiments.DomainSkillSpec.SampleSpec",
            ListMap(
              "a" -> Instance(domain.valueSpec, Seq(The("Bool"), true))
            )
          )),
          Seq.empty
        ))
      )
    )

    domain(Find(ID(Seq("SampleSpec")))) should be (
      Found(
        ID(Seq("SampleSpec")),
        Instance(domain.t, Seq(
          "SampleSpec",
          Instance(domain.spec, Seq(
            "SampleSpec",
            "dev.rudiments.DomainSkillSpec.SampleSpec",
            ListMap(
              "a" -> Instance(domain.valueSpec, Seq(The("Bool"), true))
            )
          )),
          Seq.empty
        ))
      )
    )
  }

  "update thing" in {
    domain(Update(
      ID(Seq("SampleSingleton")),
      Instance(domain.t, Seq(
        "SampleSingleton",
        Instance(domain.abs, Seq("SampleSingleton", ListMap.empty)),
        Seq.empty
      ))
    )) should be (Updated(
      ID(Seq("SampleSingleton")),
      Instance(domain.t, Seq(
        "SampleSingleton",
        Instance(domain.the, Seq("SampleSingleton")),
        Seq.empty
      )),
      Instance(domain.t, Seq(
        "SampleSingleton",
        Instance(domain.abs, Seq("SampleSingleton", ListMap.empty)),
        Seq.empty
      ))
    ))

    domain(Find(ID(Seq("SampleSingleton")))) should be (
      Found(
        ID(Seq("SampleSingleton")),
        Instance(domain.t, Seq(
          "SampleSingleton",
          Instance(domain.abs, Seq("SampleSingleton", ListMap.empty)),
          Seq.empty
        ))
      )
    )
  }

  "multiple inserts makes conflict" in {
    domain(Create(ID(Seq("SampleSingleton")), Instance(domain.the, Seq("SampleSingleton")))) should be (
      AlreadyExists(
        ID(Seq("SampleSingleton")),
        Instance(domain.t, Seq(
          "SampleSingleton",
          Instance(domain.abs, Seq("SampleSingleton", ListMap.empty)),
          Seq.empty
        ))
      )
    )
  }

  "delete thing" in {
    domain(Delete(ID(Seq("SampleSingleton")))) should be (
      Deleted(
        ID(Seq("SampleSingleton")),
        Instance(domain.t, Seq(
          "SampleSingleton",
          Instance(domain.abs, Seq("SampleSingleton", ListMap.empty)),
          Seq.empty
        ))
      )
    )
  }
}
