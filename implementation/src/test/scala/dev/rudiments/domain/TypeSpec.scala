package dev.rudiments.domain

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable.ListMap

@RunWith(classOf[JUnitRunner])
class TypeSpec extends WordSpec with Matchers {
  private implicit val domain: Domain = Domain()
  val t: Spec = domain.makeFromScala[Spec, Sample1]

  "can construct soft instance" in {
    val s = t.instantiate(domain, 1, None, Set.empty)
    s should be (Instance(t, Seq(1, None, Set.empty)))
    s.values.head should be (1)
    s.values.last should be (Set.empty)
  }

  "fail if invalid argument" in {
    assertThrows[IllegalArgumentException] {
      val i = Instance(t, Seq("1", Some("thing"), Set.empty))
      i.extract("a") // without extraction - does not fail
    }
  }

  "can extract value from soft instance" in {
    val s = t.instantiate(domain, 1, None, Set.empty)
    s.extract[Int]("a") should be (1)
    s.extract[Option[String]]("b") should be (None)
    s.extract[Set[String]]("c") should be (Set.empty)
  }

  val tt: Spec = domain.makeFromScala[Spec, Spec]
  "type of type contains ADT" in {
    tt.name should be ("Spec")
    tt.fields("fields").thing should be (
      Index(
        Plain.Text(ScalaTypes.MaxInt),
        domain.find[Spec]("ValueSpec")
      )
    )
    //ValueSpec contains Thing: ADT
  }

  "can construct type of type" in {
    val m = tt.toScala[Spec](
      "FirstSyntheticType",
      "dev.rudiments.domain.TypeSpec.FirstSyntheticType",
      ListMap(
        "firstSyntheticField" -> ValueSpec(ScalaTypes.ScalaLong, isRequired = true),
        "secondSyntheticField" -> ValueSpec(ScalaTypes.ScalaString, isRequired = false)
      )
    )
    m should be (domain.makeFromScala[Spec, FirstSyntheticType])
  }

  "synthetic type should be able to construct Instance with values of Type" in {
    val m = tt.toScala[Spec](
      "FirstSyntheticType",
      "dev.rudiments.domain.TypeSpec.FirstSyntheticType",
      ListMap(
        "firstSyntheticField" -> ValueSpec(ScalaTypes.ScalaLong, isRequired = true),
        "secondSyntheticField" -> ValueSpec(ScalaTypes.ScalaString, isRequired = false)
      )
    )
    m.instantiate(domain, 42L, Some("Because")) should be (
      Instance(m, Seq(42L, Some("Because")))
    )
  }

  case class FirstSyntheticType (
    firstSyntheticField: Long,
    secondSyntheticField: Option[String]
  ) extends DTO
}