package dev.rudiments.another

import dev.rudiments.hardcore.data.MyEnum
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class InstantiateFromScalaSpec extends WordSpec with Matchers {
  private implicit val domain: Domain = Domain()
  private val s1: Spec = domain.makeFromScala[Spec, Sample1]
  private val c1: Spec = domain.makeFromScala[Spec, Complicated1]
  private val c2: Spec = domain.makeFromScala[Spec, Complicated2]
  private val c3: Spec = domain.makeFromScala[Spec, Complicated3]


  private val enum = domain.find[Abstract]("MyEnum")

  "can construct soft instance" in {
    val s = s1.fromProduct(domain, Sample1(1, None, Set.empty))
    s should be (Instance(s1, Seq(1, None, Set.empty)))
    s.values.head should be (1)
    s.values.last should be (Set.empty)
  }

  "can extract value from soft instance" in {
    val s = s1.fromProduct(domain, Sample1(1, None, Set.empty))
    s.extract[Int]("a") should be (1)
    s.extract[Option[String]]("b") should be (None)
    s.extract[Set[String]]("c") should be (Set.empty)
  }

  "can construct instance with plain field and it's composition" in {
    val complicatedPlain = Complicated1(
      a = 42,
      b = Some(42L),
      c = Set("red", "green", "blue"),
      d = Map ("1" -> 1, "2" -> 2, "3" -> 3)
    )
    c1.fromProduct(domain, complicatedPlain) should be (
      Instance(c1, Seq(
        42,
        Some(42L),
        Set("red", "green", "blue"),
        Map("1" -> 1, "2" -> 2, "3" -> 3)
      ))
    )
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
    val v = c2.fromProduct(domain, complicatedComposite)
    c2.fromProduct(domain, complicatedComposite) should be (Instance(c2, Seq(
      Instance(s1, Seq(1, None, Set.empty)),
      Some(Instance(s1, Seq(1, None, Set.empty))),
      Set(
        Instance(s1, Seq(1, None, Set.empty)),
        Instance(s1, Seq(2, Some("thing"), Set.empty)),
        Instance(s1, Seq(3, Some("other thing"), Set("white", "black", "grey")))
      ),
      Map(
        "empty" -> Instance(s1, Seq(0, None, Set.empty)),
        "one" -> Instance(s1, Seq(1, Some("one"), Set.empty)),
        "full" -> Instance(s1, Seq(42, Some("value"), Set("sky", "ocean", "land")))
      )
    )))
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
    c3.fromProduct(domain, complicatedEnum) should be (Instance(c3, Seq(
      domain.afterParent(enum, "One"),
      Some(domain.afterParent(enum, "Two")),
      Set(domain.afterParent(enum, "Red"), domain.afterParent(enum, "Two")),
      Map(
        "1" -> domain.afterParent(enum, "One"),
        "2" -> domain.afterParent(enum, "Two"),
        "red" -> domain.afterParent(enum, "Red")
      )
    )))
  }
}
