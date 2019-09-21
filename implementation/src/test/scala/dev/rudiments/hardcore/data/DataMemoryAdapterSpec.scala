package dev.rudiments.hardcore.data

import dev.rudiments.hardcore.types.{DTO, ID, Type}
import org.junit.runner.RunWith
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.junit.JUnitRunner

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class DataMemoryAdapterSpec extends WordSpec with Matchers {
  case class Example(
    id: Long,
    name: String,
    comment: Option[String] = None
  ) extends DTO

  implicit val t: Type[Example] = Type[Example]
  val repo: DataMemoryAdapter[Example] = new DataMemoryAdapter[Example]
  val sample = Example(42, "sample")
  val id: ID[Example] = ID(sample.id)

  "no element by ID" in {
    repo(Count[Example]()) should be (Counted(0))
    repo(Find(id)) should be (NotFound(id))
  }

  "put item into repository" in {
    repo(Count[Example]()) should be (Counted(0))
    repo(Create(id, sample)) should be (Created(id, sample))
    repo(Count[Example]()) should be (Counted(1))
    repo(Find(id)) should be (Found(id, sample))
  }

  "update item in repository" in {
    repo(Update(id, sample.copy(comment = Some("changes")))) should be (
      Updated(
        id,
        Example(42, "sample"),
        Example(42, "sample", Some("changes"))))
    repo(Count[Example]()) should be (Counted(1))
    repo(Find(id)) should be (Found(id, Example(42, "sample", Some("changes"))))
  }

  "multiple inserts with same ID causes exception" in {
    repo(Count[Example]()) should be (Counted(1))
    repo(Create(id, sample)) should be (AlreadyExists(id, Example(42, "sample", Some("changes"))))
  }

  "delete item from repository" in {
    repo(Count[Example]()) should be (Counted(1))
    repo(Delete(id)) should be (Deleted(id, Example(42, "sample", Some("changes"))))
    repo(Count[Example]()) should be (Counted(0))
    repo(Find(id)) should be (NotFound(id))
  }

  "endure 100.000 records" in {
    (1 to 100000)
      .map(i => Example(i, s"$i'th element"))
      .foreach(sample => repo(Create(ID(sample.id), sample)))

    repo(Count[Example]()) should be (Counted(100000))

    val rnd = new Random().nextInt(100000)
    repo(Find(ID(rnd))) should be (Found(ID(rnd), Example(rnd, s"$rnd'th element")))
  }

  "endure 100.000 batch" in {
    val batch = (100001 to 200000).map(i => (ID[Example, Long](i), Example(i, s"$i'th element"))).toMap
    repo(CreateAll(batch)(t)) should be (AllCreated(batch)(t))

    repo(Count[Example]()) should be (Counted(200000))

    val rnd = new Random().nextInt(200000)
    repo(Find(ID[Example, Long](rnd))(t)) should be (Found(ID[Example, Long](rnd), Example(rnd, s"$rnd'th element")))
  }

  "clear repository" in {
    repo(Count[Example]()) should be (Counted(200000))
    repo(DeleteAll[Example]) should be (AllDeleted[Example]())
    repo(Count[Example]()) should be (Counted(0))
  }
}
