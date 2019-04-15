package dev.rudiments.hardcore.repo.memory.spec

import org.scalatest.{Matchers, WordSpec}
import dev.rudiments.hardcore.dsl.ID._
import dev.rudiments.hardcore.dsl._
import dev.rudiments.hardcore.repo.WriteRepository
import dev.rudiments.hardcore.repo.memory.{MemoryFKRepo, MemoryRepo}

import cats.effect.IO

import scala.util.Random

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class AggregateMemoryRepoPoC extends WordSpec with Matchers {
  case class Container(
    id: Long,
    name: String,
    parts: Set[Parts] = Set.empty
  ) {
    def plain: Container = this.copy(parts = Set.empty)
  }

  case class Parts(
    comment: String
  )

  class ContainerMemoryRepo(implicit meta: Meta[Container]) extends WriteRepository[Container] {
    private val repo = new MemoryRepo[Container]
    private val parts = new MemoryFKRepo[Container, Parts]

    override def get(id: ID[Container]): IO[Result[Container]] = {
      for {
        plain <- repo.get(id)
        parts <- parts.get(id)
      } yield Result(id, plain.value.copy(parts = parts.values.toSet))
    }

    override def find(query: Query[Container]): IO[QueryResult[Container]] = ???

    override def count(filters: Filter[Container]*): IO[Long] = repo.count(filters: _*)

    override def create(draft: Container): IO[Created[Container]] = {
      for {
        plain <- repo.create(draft.plain)
        parts <- parts.create(draft.identify, draft.parts)
      } yield Created(draft.identify, plain.value.copy(parts = parts.values.toSet))
    }

    override def update(value: Container): IO[Updated[Container]] = {
      for {
        plain <- repo.update(value.plain)
        parts <- parts.update(value.identify, value.parts)
      } yield Updated(
        value.identify,
        plain.oldValue.copy(parts = parts.oldValues.toSet),
        plain.newValue.copy(parts = parts.newValues.toSet)
      )
    }

    override def delete(id: ID[Container]): IO[Deleted[Container]] = {
      for {
        plain <- repo.get(id)
        p <- parts.get(id)
        _ <- repo.delete(id)
        _ <- parts.delete(id)
      } yield Deleted(id, plain.value.copy(parts = p.values.toSet))
    }

    override def createAll(values: Iterable[Container]): IO[BatchCreated[Container]] = {
      for {
        plain <- repo.createAll(values.map(_.plain))
        parts <- parts.createAll(values.groupBy(c => c.identify).mapValues(_.flatMap(_.parts)))
      } yield BatchCreated[Container](values)
    }

    override def deleteAll(): IO[AllDeleted[Container]] = {
      for {
        _ <- parts.deleteAll()
        _ <- repo.deleteAll()
      } yield AllDeleted[Container]()
    }
  }

  implicit val meta: Meta[Container] = Meta(value => ID(value.id))
  val repo: ContainerMemoryRepo = new ContainerMemoryRepo

  val sample = Container(42, "sample", Set(Parts("123"), Parts("456")))
  val id: ID[Container] = sample.identify

  "no element by ID" in {
    an[NotFound[Container]] should be thrownBy repo.get(id).unsafeRunSync()
  }

  "put collection into repository" in {
    repo.create(sample).unsafeRunSync() should be (Created(id, Container(42, "sample", Set(Parts("123"), Parts("456")))))
    repo.get(id).unsafeRunSync() should be (Result(id, Container(42, "sample", Set(Parts("123"), Parts("456")))))
  }

  "update collection in repository" in {
    val updated = Container(42, "example", Set(Parts("1123"), Parts("4456"), Parts("7789")))
    repo.get(id).unsafeRunSync() should be (Result(id, Container(42, "sample", Set(Parts("123"), Parts("456")))))
    repo.update(updated).unsafeRunSync() should be (Updated(id, sample, updated))
    repo.get(id).unsafeRunSync() should be (Result(id, Container(42, "example", Set(Parts("1123"), Parts("4456"), Parts("7789")))))
  }

  "multiple inserts with same ID causes exception" in {
    an[AlreadyExists[Container]] should be thrownBy repo.create(sample).unsafeRunSync()
  }

  "delete collection from repository" in {
    repo.delete(id).unsafeRunSync() should be (Deleted(id, Container(42, "example", Set(Parts("1123"), Parts("4456"), Parts("7789")))))
    an[NotFound[Container]] should be thrownBy repo.get(id).unsafeRunSync()
  }

  "endure 1.000.000 records" in {
    (1 to 10000).map { i =>
      Container(i, s"$i'th element in collection",
        (1 to 100).map(j => Parts(j.toString)).toSet
      )
    }.foreach(container => repo.create(container).unsafeRunSync())

    repo.count().unsafeRunSync() should be (10000)

    val rnd = new Random().nextInt(10000)
    repo.get(ID(rnd)).unsafeRunSync() should be (
      Result(ID(rnd), Container(
        rnd,
        s"$rnd'th element in collection",
        (1 to 100).map(j => Parts(j.toString)).toSet
      ))
    )
  }

  "endure 1.000.000 batch" in {
    val values = (10001 to 20000).map { i =>
      Container(i, s"$i'th element in collection",
        (1 to 100).map(j => Parts(j.toString)).toSet
      )
    }
    repo.createAll(values).unsafeRunSync()

    repo.count().unsafeRunSync() should be (20000)

    val rnd = new Random().nextInt(20000)
    repo.get(ID(rnd)).unsafeRunSync() should be (
      Result(ID(rnd), Container(
        rnd,
        s"$rnd'th element in collection",
        (1 to 100).map(j => Parts(j.toString)).toSet
      ))
    )
  }

  "clear repository" in {
    repo.count().unsafeRunSync() should be (20000)
    repo.deleteAll().unsafeRunSync() should be (AllDeleted[Container]())
    repo.count().unsafeRunSync() should be (0)
  }
}
