package work.unformed.hardcore.repo.memory.spec

import org.scalatest.{Matchers, WordSpec}
import work.unformed.hardcore.dsl.ID._
import work.unformed.hardcore.dsl._
import work.unformed.hardcore.repo.WriteRepository
import work.unformed.hardcore.repo.memory.{MemoryFKRepo, MemoryRepo}
import cats.implicits._

import scala.language.implicitConversions
import scala.util.Random


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

    override def get(id: ID[Container]): Either[Error[Container], Result[Container]] = {
      for {
        plain <- repo.get(id)
        parts <- parts.get(id).leftMap(Internal[Container, Parts])
      } yield Result(id, plain.value.copy(parts = parts.values.toSet))
    }

    override def find(query: Query[Container]): Either[Error[Container], QueryResult[Container]] = ???

    override def count(filters: Filter[Container]*): Long = repo.count(filters: _*)

    override def create(draft: Container): Either[Error[Container], Created[Container]] = {
      for {
        plain <- repo.create(draft.plain)
        parts <- parts.create(draft.identify, draft.parts).leftMap(Internal[Container, Parts])
      } yield Created(draft.identify, plain.value.copy(parts = parts.values.toSet))
    }

    override def update(value: Container): Either[Error[Container], Updated[Container]] = {
      for {
        plain <- repo.update(value.plain)
        parts <- parts.update(value.identify, value.parts).leftMap(Internal[Container, Parts])
      } yield Updated(
        value.identify,
        plain.oldValue.copy(parts = parts.oldValues.toSet),
        plain.newValue.copy(parts = parts.newValues.toSet)
      )
    }

    override def delete(id: ID[Container]): Either[Error[Container], Deleted[Container]] = {
      for {
        plain <- repo.get(id)
        p <- parts.get(id).leftMap(Internal[Container, Parts])
        _ <- repo.delete(id)
        _ <- parts.delete(id).leftMap(Internal[Container, Parts])
      } yield Deleted(id, plain.value.copy(parts = p.values.toSet))
    }

    override def createAll(values: Iterable[Container]): Either[Error[Container], BatchCreated[Container]] = {
      for {
        plain <- repo.createAll(values.map(_.plain))
        parts <- parts.createAll(values.groupBy(c => c.identify).mapValues(_.flatMap(_.parts)))
                      .leftMap(Internal[Container, Parts])
      } yield BatchCreated[Container](values)
    }

    override def deleteAll(): Either[Error[Container], AllDeleted[Container]] = {
      for {
        _ <- parts.deleteAll().leftMap(Internal[Container, Parts])
        _ <- repo.deleteAll()
      } yield AllDeleted[Container]()
    }
  }

  implicit val meta: Meta[Container] = Meta(value => ID(value.id))
  val repo: ContainerMemoryRepo = new ContainerMemoryRepo

  val sample = Container(42, "sample", Set(Parts("123"), Parts("456")))
  val id: ID[Container] = sample.identify

  "no element by ID" in {
    repo.get(id) should be (Left(NotFound(id)))
  }

  "put collection into repository" in {
    repo.create(sample) should be (Created(id, Container(42, "sample", Set(Parts("123"), Parts("456")))).asRight)
    repo.get(id) should be (Result(id, Container(42, "sample", Set(Parts("123"), Parts("456")))).asRight)
  }

  "update collection in repository" in {
    val updated = Container(42, "example", Set(Parts("1123"), Parts("4456"), Parts("7789")))
    repo.get(id) should be (Result(id, Container(42, "sample", Set(Parts("123"), Parts("456")))).asRight)
    repo.update(updated) should be (Updated(id, sample, updated).asRight)
    repo.get(id) should be (Result(id, Container(42, "example", Set(Parts("1123"), Parts("4456"), Parts("7789")))).asRight)
  }

  "multiple inserts with same ID causes exception" in {
    repo.create(sample) should be (AlreadyExists(id).asLeft)
  }

  "delete collection from repository" in {
    repo.delete(id) should be (Deleted(id, Container(42, "example", Set(Parts("1123"), Parts("4456"), Parts("7789")))).asRight)
    repo.get(id) should be (Left(NotFound(id)))
  }

  "endure 1.000.000 records" in {
    (1 to 10000).map { i =>
      Container(i, s"$i'th element in collection",
        (1 to 100).map(j => Parts(j.toString)).toSet
      )
    }.foreach(container => repo.create(container))

    repo.count() should be (10000)

    val rnd = new Random().nextInt(10000)
    repo.get(ID(rnd)) should be (
      Result(ID(rnd), Container(
        rnd,
        s"$rnd'th element in collection",
        (1 to 100).map(j => Parts(j.toString)).toSet
      )).asRight
    )
  }

  "endure 1.000.000 batch" in {
    val values = (10001 to 20000).map { i =>
      Container(i, s"$i'th element in collection",
        (1 to 100).map(j => Parts(j.toString)).toSet
      )
    }
    repo.createAll(values)

    repo.count() should be (20000)

    val rnd = new Random().nextInt(20000)
    repo.get(ID(rnd)) should be (
      Result(ID(rnd), Container(
        rnd,
        s"$rnd'th element in collection",
        (1 to 100).map(j => Parts(j.toString)).toSet
      )).asRight
    )
  }

  "clear repository" in {
    repo.count() should be (20000)
    repo.deleteAll() should be (AllDeleted[Container]().asRight)
    repo.count() should be (0)
  }
}
