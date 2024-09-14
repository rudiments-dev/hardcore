package test.dev.rudiments.hardcore

import dev.rudiments.hardcore.Tree
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TreeTest extends AnyWordSpec with Matchers {
  "can make empty tree" in {
    val t = Tree.empty[Int, Int]
    t.self should be (())
    t.items shouldBe(empty)
  }

  var t: Tree[Int, Unit, String] = _
  "can make nested trees with leaves" in {
    t = Tree(
      1 -> "a", 2 -> "b",
      3 -> Tree(
        4 -> "c",
        5 -> Tree(
          6 -> "d",
          7 -> "e"
        ),
        8 -> Tree(
          9 -> "f"
        ),
        10 -> "g"
      ),
      11 -> "h"
    )

    t.self should be(())
    t.items.size should be(4)
  }

  "can make a deep search in nested tree" in {
    t.deep should be(Seq(
      List.empty[Int] -> (),
      List(1) -> "a", List(2) -> "b",
      List(3) -> (),
      List(3, 4) -> "c",
      List(3, 5) -> (),
      List(3, 5, 6) -> "d",
      List(3, 5, 7) -> "e",
      List(3, 8) -> (),
      List(3, 8, 9) -> "f",
      List(3, 10) -> "g",
      List(11) -> "h"
    ))
  }

  "can make a wide search in nested tree" in {
    t.wide should be (Seq(
      List.empty[Int] -> (),
      List(1) -> "a", List(2) -> "b",
      List(3) -> (),
      List(11) -> "h",
      List(3, 4) -> "c",
      List(3, 5) -> (),
      List(3, 8) -> (),
      List(3, 10) -> "g",
      List(3, 5, 6) -> "d",
      List(3, 5, 7) -> "e",
      List(3, 8, 9) -> "f",
    ))
  }
}
