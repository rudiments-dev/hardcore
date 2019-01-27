package work.unformed.hardcore.repo.memory.spec

import cats.effect.{IO, Resource}
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
    } yield second

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

  def mkResource(s: String) = {
    val acquire = IO(println(s"Acquiring $s")) *> IO.pure(s)

    def release(s: String) = IO(println(s"Releasing $s"))

    Resource.make(acquire)(release)
  }
  "composing resource like IO2" in {

    val r = for {
      outer <- mkResource("outer")
      inner <- mkResource("inner")
    } yield (outer, inner)

    r.use { case (a, b) => IO(println(s"Using $a and $b")) }.unsafeRunSync
    1 should be (1)

  }
}
