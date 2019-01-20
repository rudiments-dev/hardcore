package work.unformed.hardcore.repo.memory.spec

import cats.effect.IO
import org.scalatest.{Matchers, WordSpec}
import work.unformed.hardcore.dsl.ID._
import work.unformed.hardcore.dsl._
import work.unformed.hardcore.repo.WriteRepository
import work.unformed.hardcore.repo.memory.{MemoryFKRepo, MemoryRepo}

import scala.util.Random


class AggregateMemoryRepoPoC extends WordSpec with Matchers {
  case class Container(
    id: Long,
    name: String,
    parts: Set[Parts] = Set.empty
  )

  case class Parts(
    comment: String
  )

  class ContainerMemoryRepo(implicit meta: Meta[Container]) extends WriteRepository[Container] {
    private val repo = new MemoryRepo[Container]
    private val parts = new MemoryFKRepo[Container, Parts]


    override def get(id: ID[Container]): IO[Option[Container]] = for {
      base <- repo.get(id)
      parts <- parts.get(id)
    } yield base.map(b => b.copy(parts = parts.toSet))

    override def create(draft: Container): IO[Container] = for {
        base  <- repo.create(draft.copy(parts = Set.empty))
        parts <- parts.create(draft.identify, draft.parts)
    } yield base.copy(parts = parts.toSet)

    override def delete(id: ID[Container]): IO[Unit] = for {
      _ <- parts.delete(id)
      _ <- repo.delete(id)
    } yield ()

    override def deleteAll(): IO[Unit] = for {
      _ <- parts.deleteAll()
      _ <- repo.deleteAll()
    } yield ()

    override def update(value: Container): IO[Container] = {
      get(value.identify).flatMap {
        case Some(_) =>
          for {
            base <- repo.update(value.copy(parts = Set.empty))
            parts <- parts.update(value.identify, value.parts)
          } yield base.copy(parts = parts.toSet)
        case None => throw new IllegalArgumentException(s"${value.identify} not exists, can't update")
      }
    }



    override def find(query: Query[Container]): Result[Container] = ???

    override def count(filters: Filter[Container]*): Long = repo.count(filters: _*)

    override def values(field: String, filters: Filter[Container]*): Unit = ???
  }

  implicit val meta: Meta[Container] = Meta(value => ID(value.id))
  val repo: ContainerMemoryRepo = new ContainerMemoryRepo

  val sample = Container(42, "sample", Set(Parts("123"), Parts("456")))
  val id: ID[Container] = sample.identify

  "no element by ID" in {
    repo.get(id).unsafeRunSync() should be (None)
    an[RuntimeException] should be thrownBy repo.strict(id).unsafeRunSync()
  }

  "put collection into repository" in {
    repo.create(sample).unsafeRunSync() should be (Container(42, "sample", Set(Parts("123"), Parts("456"))))
    repo.strict(id).unsafeRunSync() should be (Container(42, "sample", Set(Parts("123"), Parts("456"))))
  }

  "update collection in repository" in {
    val updated = Container(42, "example", Set(Parts("1123"), Parts("4456"), Parts("7789")))
    repo.strict(id).unsafeRunSync() should be (Container(42, "sample", Set(Parts("123"), Parts("456"))))
    repo.update(updated).unsafeRunSync() should be (Container(42, "example", Set(Parts("1123"), Parts("4456"), Parts("7789"))))
    repo.strict(id).unsafeRunSync() should be (Container(42, "example", Set(Parts("1123"), Parts("4456"), Parts("7789"))))
  }

  "multiple inserts with same ID causes exception" in {
    an[RuntimeException] should be thrownBy repo.create(sample).unsafeRunSync()
  }

  "delete collection from repository" in {
    repo.delete(id).unsafeRunSync()
    repo.get(id).unsafeRunSync() should be (None)
  }

  "endure 1.000.000 records" in {
    (1 to 10000).map { i =>
      Container(i, s"$i'th element in collection",
        (1 to 100).map(j => Parts(j.toString)).toSet
      )
    }.foreach(container => repo.create(container).unsafeRunSync())

    repo.count() should be (10000)

    val rnd = new Random().nextInt(10000)
    repo.strict(ID(rnd)).unsafeRunSync() should be (Container(rnd, s"$rnd'th element in collection",
      (1 to 100).map(j => Parts(j.toString)).toSet
    ))
  }

  "endure 1.000.000 batch" in {
    val values = (10001 to 20000).map { i =>
      Container(i, s"$i'th element in collection",
        (1 to 100).map(j => Parts(j.toString)).toSet
      )
    }
    repo.createAll(values).unsafeRunSync()

    repo.count() should be (20000)

    val rnd = new Random().nextInt(20000)
    repo.strict(ID(rnd)).unsafeRunSync() should be (Container(rnd, s"$rnd'th element in collection",
      (1 to 100).map(j => Parts(j.toString)).toSet
    ))
  }

  "clear repository" in {
    repo.count() should be (20000)
    repo.deleteAll().unsafeRunSync()
    repo.count() should be (0)
  }
}
