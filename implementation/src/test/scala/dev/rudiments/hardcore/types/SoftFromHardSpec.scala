package dev.rudiments.hardcore.types

import dev.rudiments.hardcore.data.MyEnum
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class SoftFromHardSpec extends WordSpec with Matchers {
  private val s1: Type = HardType[Sample1]
  private val c1: Type = HardType[Complicated1]
  private val c2: Type = HardType[Complicated2]
  private val c3: Type = HardType[Complicated3]

  private val softEnum = Types.Enum("dev.rudiments.hardcore.data.MyEnum", Seq("One", "Two", "Red"))

  "can construct soft instance" in {
    val s = s1.softFromHard(Sample1(1, None, Set.empty))
    s should be (SoftInstance(Map("a" -> 1, "b" -> None, "c" -> Set.empty))(s1))
    s.fields.head should be ("a" -> 1)
    s.fields.last should be ("c" -> Set.empty)
  }

  "can extract value from soft instance" in {
    val s = s1.softFromHard(Sample1(1, None, Set.empty))
    s1.extract(s, "a") should be (1)
    s1.extract(s, "b") should be (None)
    s1.extract(s, "c") should be (Set.empty)
  }

  "can construct instance with plain field and it's composition" in {
    val complicatedPlain = Complicated1(
      a = 42,
      b = Some(42L),
      c = Set("red", "green", "blue"),
      d = Map ("1" -> 1, "2" -> 2, "3" -> 3)
    )
    c1.softFromHard(complicatedPlain) should be (SoftInstance(
      42, Some(42L), Set("red", "green", "blue"), Map ("1" -> 1, "2" -> 2, "3" -> 3)
    )(c1))
  }

  "can construct instance with nested type field and it's composition" in {
    val complicatedComposite = Complicated2(
      e = Sample1(1, None, Set.empty),
      f = Some(Sample1(1, None, Set.empty)),
      g = Set(
        Sample1(1, None, Set.empty),
        Sample1(2, Some("thing"), Set.empty),
        Sample1(3, Some("other thing"), Set("white", "black", "grey"))
      ),
      h = Map(
        "empty" -> Sample1(0, None, Set.empty),
        "one" -> Sample1(1, Some("one"), Set.empty),
        "full" -> Sample1(42, Some("value"), Set("sky", "ocean", "land"))
      )
    )
    c2.softFromHard(complicatedComposite) should be (SoftInstance(
      SoftInstance(1, None, Set.empty)(s1),
      Some(SoftInstance(1, None, Set.empty)(s1)),
      Set(
        SoftInstance(1, None, Set.empty)(s1),
        SoftInstance(2, Some("thing"), Set.empty)(s1),
        SoftInstance(3, Some("other thing"), Set("white", "black", "grey"))(s1)
      ),
      Map(
        "empty" -> SoftInstance(0, None, Set.empty)(s1),
        "one" -> SoftInstance(1, Some("one"), Set.empty)(s1),
        "full" -> SoftInstance(42, Some("value"), Set("sky", "ocean", "land"))(s1)
      )
    )(c2))
  }

  "can construct instance with enum field and it's composition" in {
    val complicatedEnum = Complicated3(
      i = MyEnum.One,
      j = Some(MyEnum.Two),
      k = Set(MyEnum.Red, MyEnum.Two),
      l = Map(
        "1" -> MyEnum.One,
        "2" -> MyEnum.Two,
        "red" -> MyEnum.Red
      )
    )
    c3.softFromHard(complicatedEnum) should be (SoftInstance(
      SoftEnum(softEnum, 0),
      Some(SoftEnum(softEnum, 1)),
      Set(
        SoftEnum(softEnum, 1),
        SoftEnum(softEnum, 2)
      ),
      Map(
        "1" -> SoftEnum(softEnum, 0),
        "2" -> SoftEnum(softEnum, 1),
        "red" -> SoftEnum(softEnum, 2)
      )
    )(c3))
  }
}

case class Complicated1(
  a: Int,
  b: Option[Long],
  c: Set[String],
  d: Map[String, Int]
)

case class Complicated2(
  e: Sample1,
  f: Option[Sample1],
  g: Set[Sample1],
  h: Map[String, Sample1],
)

case class Complicated3(
  i: MyEnum,
  j: Option[MyEnum],
  k: Set[MyEnum],
  l: Map[String, MyEnum]
)