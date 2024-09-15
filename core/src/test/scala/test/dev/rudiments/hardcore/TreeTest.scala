package test.dev.rudiments.hardcore

import dev.rudiments.hardcore.{ Created, Deleted, LeafOnTheWay, NotFound, Tree, Updated }
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TreeTest extends AnyWordSpec with Matchers {
  "can make empty tree" in {
    val t = Tree.empty[Int, Int]
    t.self should be (())
    t.items shouldBe empty
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

  "can read from a nested trees" in {
    t.read(1 :: Nil) should be (Right("a"))
    t.read(3 :: 4 :: Nil) should be (Right("c"))
    t.read(3 :: 8 :: 9 :: Nil) should be (Right("f"))

    t.read(3 :: 4 :: 5 :: Nil) should be (Left(LeafOnTheWay(4, 5 :: Nil)))
    t.read(42 :: Nil) should be (Left(NotFound(42 :: Nil)))

    t.read(3 :: 8 :: Nil) should be (Right(Tree(
      9 -> "f"
    )))
    t.read(3 :: Nil) should be(Right(Tree(
      4 -> "c",
      5 -> Tree(
        6 -> "d",
        7 -> "e"
      ),
      8 -> Tree(
        9 -> "f"
      ),
      10 -> "g"
    )))
    t.read(Nil) should be (Right(Tree(
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
    )))
  }

  "can make a deep search in nested trees" in {
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

  "can make a wide search in nested trees" in {
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

  "can create element" in {
    t.read(12 :: Nil) should be (Left(NotFound(12 :: Nil)))

    t = t.apply(12, Created("k"))

    t should be (Tree(
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
      11 -> "h", 12 -> "k"
    ))

    t.read(13 :: Nil) should be (Left(NotFound(13 :: Nil)))

    t = t.apply(13, Created(Tree(14 -> "l")))

    t should be(Tree(
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
      11 -> "h", 12 -> "k",
      13 -> Tree(14 -> "l")
    ))
  }

  "can update element" in {
    t.read(12) should be (Right("k"))
    t = t.apply(12, Updated("k", "j"))
    t.read(12) should be (Right("j"))

    t.read(13) should be (Right(Tree( 14 -> "l" )))
    t = t.apply(13, Updated(
      Tree( 14 -> "l" ),
      Tree( 15 -> Tree( 16 -> "m" ), 17 -> "n" ))
    )
    t.read(13) should be (Right(Tree(
      15 -> Tree( 16 -> "m" ),
      17 -> "n"
    )))
    t should be (Tree(
      1 -> "a", 2 -> "b",
      3 -> Tree(
        4 -> "c",
        5 -> Tree( 6 -> "d", 7 -> "e" ),
        8 -> Tree( 9 -> "f" ),
        10 -> "g"
      ),
      11 -> "h", 12 -> "j",
      13 -> Tree(
        15 -> Tree( 16 -> "m" ),
        17 -> "n"
      )
    ))
  }

  "can delete element" in {
    t = t.apply(12, Deleted("j"))
    t.read(12) should be (Left(NotFound(12 :: Nil)))

    t = t.apply(13, Deleted(Tree(
      15 -> Tree( 16 -> "m" ),
      17 -> "n"
    )))
    t.read(13) should be (Left(NotFound(13 :: Nil)))

    t should be(Tree(
      1 -> "a", 2 -> "b",
      3 -> Tree(
        4 -> "c",
        5 -> Tree(6 -> "d", 7 -> "e"),
        8 -> Tree(9 -> "f"),
        10 -> "g"
      ),
      11 -> "h"
    ))
  }
}
