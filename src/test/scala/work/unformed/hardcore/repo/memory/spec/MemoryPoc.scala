package work.unformed.hardcore.repo.memory.spec

import cats.effect.{IO, Resource, Bracket}
import org.scalatest.{Matchers, WordSpec}
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging


class MemoryPoc extends WordSpec with Matchers with LazyLogging {


  val resourceOne = Resource.make {
    IO { println("pre one") }  *> IO.pure("one")
  }{
    _ => IO { println("post one") }
  }

  val resourceTwo = Resource.make {
    IO { println("pre two") } *> IO.pure("two")
  }{
    _ => IO { println("post two") }
  }

  "composing resource like IO" in {

    val a: IO[Unit] = for {
      one <- resourceOne.use(_ => IO { println("act one") })
      second <- resourceTwo.use(_ => IO { println("act two")} )
    } yield IO.unit

    a.unsafeRunSync()

    1 should be (1)
  }

  "composing resource" in {

    val a = for {
      one <- resourceOne
      two <- resourceTwo
    } yield (one, two)

    a.use(_ => IO{ println("action")} ).unsafeRunSync()

    1 should be (1)
  }

  "composing resources" in {

    val r = for {
      outer <- resourceOne
      inner <- resourceTwo
    } yield (outer, inner)

    r.use { case (a, b) => IO(println(s"Using $a and $b")) }.unsafeRunSync
    1 should be (1)
  }

  val a1 = IO { println("pre one") }  *> IO.pure("one")
  val a2 = IO { println("pre two") }  *> IO.pure("two")


  "composing resource like IO2" in {

    val a1WithBracket: IO[String] = a1.bracket {
      s => IO {s"action $s"}
    } {
      s => IO { println(s"post $s") }
    }
    val a2WithBracket: IO[String] = a2.bracket {
      s => IO {s"action $s"}
    } {
      s => IO { println(s"post $s") }
    }

    (a1WithBracket, a2WithBracket).tupled.unsafeRunSync
    1 should be (1)
  }

}
