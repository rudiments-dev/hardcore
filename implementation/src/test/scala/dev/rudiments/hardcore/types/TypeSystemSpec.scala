package dev.rudiments.hardcore.types

import org.junit.runner.RunWith
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TypeSystemSpec extends WordSpec with Matchers {
  val t: HardType[Example] = HardType[Example]
  val s1: HardType[Sample1] = HardType[Sample1]
  val s2: HardType[Sample2] = HardType[Sample2]
  val s3: HardType[Sample3] = HardType[Sample3]
  val typeSystem: TypeSystem = TypeSystem("sample", t, s1, s2, s3)

  "types are in type system" in {
    typeSystem.types.head should be ("Example" -> t)
    typeSystem.types.last should be ("Sample3" -> s3)
    typeSystem.types should be(Map(
      "Example" -> t,
      "Sample1" -> s1,
      "Sample2" -> s2,
      "Sample3" -> s3,
    ))
  }
}

case class Sample1 (
  a: Int,
  b: Option[String] = None,
  c: Set[String] = Set.empty
) extends DTO

case class Sample2 (
  a: Int,
  b: Option[String] = None,
  c: Set[String] = Set.empty
) extends DTO

case class Sample3 (
  a: Int,
  b: Option[String] = None,
  c: Set[String] = Set.empty
) extends DTO