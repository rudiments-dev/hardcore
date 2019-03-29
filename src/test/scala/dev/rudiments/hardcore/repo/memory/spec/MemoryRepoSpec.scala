package dev.rudiments.hardcore.repo.memory.spec

import org.scalatest.{Matchers, WordSpec}
import dev.rudiments.hardcore.dsl._
import dev.rudiments.hardcore.dsl.ID._
import dev.rudiments.hardcore.repo.memory.MemoryRepo

import scala.util.Random

class MemoryRepoSpec extends WordSpec with Matchers {
  case class Example(
    id: Long,
    name: String,
    comment: Option[String] = None
  )

  implicit val meta: Meta[Example] = Meta(value => ID(value.id))
  val repo: MemoryRepo[Example] = new MemoryRepo[Example]
  val sample = Example(42, "sample")
  val id = sample.identify

  "no element by ID" in {
    repo.count().unsafeRunSync() should be (0)
    an[NotFound[Example]] should be thrownBy repo.get(id).unsafeRunSync()
  }

  "put item into repository" in {
    repo.count().unsafeRunSync() should be (0)
    repo.create(sample).unsafeRunSync() should be (Created(id, sample))
    repo.count().unsafeRunSync() should be (1)
    repo.get(id).unsafeRunSync() should be (Result(id, sample))
  }

  "update item in repository" in {
    repo.update(sample.copy(comment = Some("changes"))).unsafeRunSync() should be (
      Updated(
        id,
        Example(42, "sample"),
        Example(42, "sample", Some("changes"))))
    repo.count().unsafeRunSync() should be (1)
    repo.get(id).unsafeRunSync() should be (Result(id, Example(42, "sample", Some("changes"))))
  }

  "multiple inserts with same ID causes exception" in {
    repo.count().unsafeRunSync() should be (1)
    an[AlreadyExists[Example]] should be thrownBy repo.create(sample).unsafeRunSync()
  }

  "delete item from repository" in {
    repo.count().unsafeRunSync() should be (1)
    repo.delete(id).unsafeRunSync() should be (Deleted(id, Example(42, "sample", Some("changes"))))
    repo.count().unsafeRunSync() should be (0)

    an[NotFound[Example]] should be thrownBy repo.get(id).unsafeRunSync()
  }

  "endure 100.000 records" in {
    (1 to 100000)
      .map(i => Example(i, s"$i'th element"))
      .foreach(repo.create(_).unsafeRunSync())

    repo.count().unsafeRunSync() should be (100000)

    val rnd = new Random().nextInt(100000)
    repo.get(ID(rnd)).unsafeRunSync() should be (Result(ID(rnd), Example(rnd, s"$rnd'th element")))
  }

  "endure 100.000 batch" in {
    val batch = (100001 to 200000).map(i => Example(i, s"$i'th element"))
    repo.createAll(batch).unsafeRunSync() should be (BatchCreated(batch))

    repo.count().unsafeRunSync() should be (200000)

    val rnd = new Random().nextInt(200000)
    repo.get(ID(rnd)).unsafeRunSync() should be (Result(ID(rnd), Example(rnd, s"$rnd'th element")))
  }

  "clear repository" in {
    repo.count().unsafeRunSync() should be (200000)
    repo.deleteAll().unsafeRunSync() should be (AllDeleted[Example]())
    repo.count().unsafeRunSync() should be (0)
  }

}
