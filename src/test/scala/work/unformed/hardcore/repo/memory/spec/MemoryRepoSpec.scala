package work.unformed.hardcore.repo.memory.spec

import org.scalatest.{Matchers, WordSpec}
import work.unformed.hardcore.dsl._
import work.unformed.hardcore.dsl.ID._
import work.unformed.hardcore.repo.memory.MemoryRepo
import cats.implicits._

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
    repo.count() should be (0)
    repo.get(id) should be (Left(NotFound(id)))
  }

  "put item into repository" in {
    repo.count() should be (0)
    repo.create(sample) should be (Right(Created(id, sample)))
    repo.count() should be (1)
    repo.get(id) should be (Right(Result(id, sample)))
  }

  "update item in repository" in {
    repo.update(sample.copy(comment = Some("changes"))) should be (
      Right(Updated(
        id,
        Example(42, "sample"),
        Example(42, "sample", Some("changes")))))
    repo.count() should be (1)
    repo.get(id) should be (Right(Result(id, Example(42, "sample", Some("changes")))))
  }

  "multiple inserts with same ID causes exception" in {
    repo.count() should be (1)
    repo.create(sample) should be (Left(AlreadyExists(id)))
  }

  "delete item from repository" in {
    repo.count() should be (1)
    repo.delete(id) should be (Right(Deleted(id, Example(42, "sample", Some("changes")))))
    repo.count() should be (0)
    repo.get(id) should be (Left(NotFound(id)))
  }

  "endure 100.000 records" in {
    (1 to 100000)
      .map(i => Example(i, s"$i'th element"))
      .foreach(repo.create)

    repo.count() should be (100000)

    val rnd = new Random().nextInt(100000)
    repo.get(ID(rnd)) should be (Right(Result(ID(rnd), Example(rnd, s"$rnd'th element"))))
  }

  "endure 100.000 batch" in {
    val batch = (100001 to 200000).map(i => Example(i, s"$i'th element"))
    repo.createAll(batch) should be (Right(BatchCreated(batch)))

    repo.count() should be (200000)

    val rnd = new Random().nextInt(200000)
    repo.get(ID(rnd)) should be (Right(Result(ID(rnd), Example(rnd, s"$rnd'th element"))))
  }

  "clear repository" in {
    repo.count() should be (200000)
    repo.deleteAll() should be (Right(AllDeleted[Example]()))
    repo.count() should be (0)
  }

}
