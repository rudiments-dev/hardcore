package work.unformed.hardcore.repo.memory.spec

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

    override def get(id: ID[Container]): Option[Container] = {
      repo.get(id).map(_.copy(parts = parts.get(id).toSet))
    }

    override def find(query: Query[Container]): Result[Container] = ???

    override def count(filters: Filter[Container]*): Long = repo.count(filters: _*)

    override def values(field: String, filters: Filter[Container]*): Unit = ???

    override def create(draft: Container): Container = {
      repo.create(draft.copy(parts = Set.empty))
          .copy(parts = parts.create(draft.identify, draft.parts).toSet)
    }

    override def update(value: Container): Container = get(value.identify) match {
      case Some(_) =>
        repo.update(value.copy(parts = Set.empty))
          .copy(parts = parts.update(value.identify, value.parts).toSet)
      case None =>
        throw new IllegalArgumentException(s"${value.identify} not exists, can't update")
    }

    override def delete(id: ID[Container]): Unit = {
      parts.delete(id)
      repo.delete(id)
    }

    override def deleteAll(): Unit = {
      parts.deleteAll()
      repo.deleteAll()
    }
  }

  implicit val meta: Meta[Container] = Meta(value => ID(value.id))
  val repo: ContainerMemoryRepo = new ContainerMemoryRepo

  val sample = Container(42, "sample", Set(Parts("123"), Parts("456")))
  val id: ID[Container] = sample.identify

  "no element by ID" in {
    repo.get(id) should be (None)
    an[RuntimeException] should be thrownBy repo.strict(id)
  }

  "put collection into repository" in {
    repo.create(sample) should be (Container(42, "sample", Set(Parts("123"), Parts("456"))))
    repo.strict(id) should be (Container(42, "sample", Set(Parts("123"), Parts("456"))))
  }

  "update collection in repository" in {
    val updated = Container(42, "example", Set(Parts("1123"), Parts("4456"), Parts("7789")))
    repo.strict(id) should be (Container(42, "sample", Set(Parts("123"), Parts("456"))))
    repo.update(updated) should be (Container(42, "example", Set(Parts("1123"), Parts("4456"), Parts("7789"))))
    repo.strict(id) should be (Container(42, "example", Set(Parts("1123"), Parts("4456"), Parts("7789"))))
  }

  "multiple inserts with same ID causes exception" in {
    an[RuntimeException] should be thrownBy repo.create(sample)
  }

  "delete collection from repository" in {
    repo.delete(id)
    repo.get(id) should be (None)
  }

  "endure 1.000.000 records" in {
    (1 to 10000).map { i =>
      Container(i, s"$i'th element in collection",
        (1 to 100).map(j => Parts(j.toString)).toSet
      )
    }.foreach(container => repo.create(container))

    repo.count() should be (10000)

    val rnd = new Random().nextInt(10000)
    repo.strict(ID(rnd)) should be (Container(rnd, s"$rnd'th element in collection",
      (1 to 100).map(j => Parts(j.toString)).toSet
    ))
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
    repo.strict(ID(rnd)) should be (Container(rnd, s"$rnd'th element in collection",
      (1 to 100).map(j => Parts(j.toString)).toSet
    ))
  }

  "clear repository" in {
    repo.count() should be (20000)
    repo.deleteAll()
    repo.count() should be (0)
  }
}
