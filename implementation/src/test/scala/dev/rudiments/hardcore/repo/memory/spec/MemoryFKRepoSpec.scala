package dev.rudiments.hardcore.repo.memory.spec

import org.scalatest.{Matchers, WordSpec}
import dev.rudiments.hardcore.dsl.ID._
import dev.rudiments.hardcore.dsl._
import dev.rudiments.hardcore.repo.memory.MemoryFKRepo

import scala.util.Random

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class MemoryFKRepoSpec extends WordSpec with Matchers {
  case class Container(
    id: Long,
    name: String,
    parts: Set[Parts] = Set.empty
  )

  case class Parts(
    comment: String
  )

  implicit val meta: Meta[Container] = Meta(value => ID(value.id))
  val repo: MemoryFKRepo[Container, Parts] = new MemoryFKRepo
  val sample = Container(42, "sample", Set(Parts("123"), Parts("456")))
  val id: ID[Container] = sample.identify

  "no element by ID" in {
    repo.get(id).unsafeRunSync() should be (FKResult[Container, Parts](id, Iterable.empty))
  }

  "put collection into repository" in {
    repo.create(id, sample.parts).unsafeRunSync() should be (FKCreated(id, Set(Parts("123"), Parts("456"))))
    repo.get(id).unsafeRunSync() should be (FKResult(id, Set(Parts("123"), Parts("456"))))
  }

  "update collection in repository" in {
    repo.get(id).unsafeRunSync() should be (FKResult(id, Set(Parts("123"), Parts("456"))))
    repo.update(id, List(Parts("1123"), Parts("4456"), Parts("7789"))).unsafeRunSync() should be (
      FKUpdated(id, Set(Parts("123"), Parts("456")), List(Parts("1123"), Parts("4456"), Parts("7789")))
    )
    repo.get(id).unsafeRunSync() should be (FKResult(id, List(Parts("1123"), Parts("4456"), Parts("7789"))))
  }

  "multiple inserts with same ID causes exception" in {
    an[FKAlreadyExists[Container, Parts]] should be thrownBy repo.create(id, sample.parts).unsafeRunSync()
  }

  "delete collection from repository" in {
    repo.delete(id).unsafeRunSync() should be (FKDeleted(id, List(Parts("1123"), Parts("4456"), Parts("7789"))))
    repo.get(id).unsafeRunSync() should be (FKResult[Container, Parts](id, Iterable.empty))
  }

  "endure 1.000.000 records" in {
    (1 to 10000).map { i =>
      Container(i, s"$i'th element in collection",
        (1 to 100).map(j => Parts(j.toString)).toSet
      )
    }.foreach(container => repo.create(container.identify, container.parts).unsafeRunSync())

    repo.count().unsafeRunSync() should be (10000)

    val rnd = new Random().nextInt(10000)
    repo.get(ID(rnd)).unsafeRunSync() should be (FKResult(ID(rnd), (1 to 100).map(j => Parts(j.toString)).toSet))
  }

  "endure 1.000.000 batch" in {
    val values = (10001 to 20000).map { i =>
      Container(i, s"$i'th element in collection",
        (1 to 100).map(j => Parts(j.toString)).toSet
      )
    }.map(container => (ID[Container, Long](container.id), container.parts)).toMap
    repo.createAll(values).unsafeRunSync() should be (FKBatchCreated(values))

    repo.count().unsafeRunSync() should be (20000)

    val rnd = new Random().nextInt(20000)
    repo.get(ID(rnd)).unsafeRunSync() should be (FKResult(ID(rnd), (1 to 100).map(j => Parts(j.toString)).toSet))
  }

  "clear repository" in {
    repo.count().unsafeRunSync() should be (20000)
    repo.deleteAll().unsafeRunSync() should be (FKAllDeleted[Container, Parts]())
    repo.count().unsafeRunSync() should be (0)
  }
}
