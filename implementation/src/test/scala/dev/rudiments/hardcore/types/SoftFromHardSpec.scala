package dev.rudiments.hardcore.types

import dev.rudiments.hardcore.data.MyEnum
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class SoftFromHardSpec extends WordSpec with Matchers {
  private implicit val typeSystem: TypeSystem = new TypeSystem()
  private val s1: Type = ScalaType[Sample1]
  private val c1: Type = ScalaType[Complicated1]
  private val c2: Type = ScalaType[Complicated2]
  private val c3: Type = ScalaType[Complicated3]

  private val softEnum = new Enum(
    "dev.rudiments.hardcore.data.MyEnum",
    Declaration("MyEnum"),
    Seq(
      Singleton("One"),
      Singleton("Two"),
      Singleton("Red")
    )
  )

  "can construct soft instance" in {
    val s = s1.fromScala(Sample1(1, None, Set.empty)).asInstanceOf[SoftInstance]
    s should be (SoftInstance(Map("a" -> 1, "b" -> None, "c" -> Set.empty))(s1))
    s.fields.head should be ("a" -> 1)
    s.fields.last should be ("c" -> Set.empty)
  }

  "can extract value from soft instance" in {
    val s = s1.fromScala(Sample1(1, None, Set.empty))
    s.extract[Any]("a") should be (1)
    s.extract[Any]("b") should be (None)
    s.extract[Any]("c") should be (Set.empty)
  }

  "can construct instance with plain field and it's composition" in {
    val complicatedPlain = Complicated1(
      a = 42,
      b = Some(42L),
      c = Set("red", "green", "blue"),
      d = Map ("1" -> 1, "2" -> 2, "3" -> 3)
    )
    c1.fromScala(complicatedPlain) should be (SoftInstance(
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
    c2.fromScala(complicatedComposite) should be (SoftInstance(
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
    c3.fromScala(complicatedEnum) should be (SoftInstance(
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
