package dev.rudiments.data

import dev.rudiments.data.Batch._
import dev.rudiments.data.CRUD._
import dev.rudiments.data.ReadOnly._
import dev.rudiments.hardcore.types._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class SoftCacheSpec extends WordSpec with Matchers {
  private case class Example(
    id: Long,
    name: String,
    comment: Option[String] = None
  ) extends DTO

  private implicit val t: Type = HardType[Example]
  private val cache: SoftCache = new SoftCache
  private val sample = SoftInstance(42, "sample", None)
  private val id: ID = SoftID(t.extract(sample, "id"))

  "no element by ID" in {
    cache(Count) should be (Counted(0))
    cache(Find(id)) should be (NotFound(id))
  }

  "put item into repository" in {
    cache(Count) should be (Counted(0))
    cache(Create(id, sample)) should be (Created(id, sample))
    cache(Count) should be (Counted(1))
    cache(Find(id)) should be (Found(id, sample))
  }

  "update item in repository" in {
    cache(Update(id, SoftInstance(42, "sample", Some("changes")))) should be (
      Updated(
        id,
        SoftInstance(42, "sample", None),
        SoftInstance(42, "sample", Some("changes"))))
    cache(Count) should be (Counted(1))
    cache(Find(id)) should be (Found(id, SoftInstance(42, "sample", Some("changes"))))
  }

  "multiple inserts with same ID causes exception" in {
    cache(Count) should be (Counted(1))
    cache(Create(id, sample)) should be (AlreadyExists(id, SoftInstance(42, "sample", Some("changes"))))
  }

  "delete item from repository" in {
    cache(Count) should be (Counted(1))
    cache(Delete(id)) should be (Deleted(id, SoftInstance(42, "sample", Some("changes"))))
    cache(Count) should be (Counted(0))
    cache(Find(id)) should be (NotFound(id))
  }

  "endure 100.000 records" in {
    (1 to 100000)
      .map(i => SoftInstance(i, s"$i'th element", None))
      .foreach(s => cache(Create(SoftID(t.extract(s, "id")), s)))

    cache(Count) should be (Counted(100000))

    val rnd = new Random().nextInt(100000)
    cache(Find(SoftID(rnd))) should be (Found(SoftID(rnd), SoftInstance(rnd, s"$rnd'th element", None)))
  }

  "endure 100.000 batch" in {
    val batch = (100001 to 200000).map(i => (SoftID(i).asInstanceOf[ID], SoftInstance(i, s"$i'th element", None))).toMap
    cache(CreateAll(batch)) should be (AllCreated(batch))

    cache(Count) should be (Counted(200000))

    val rnd = new Random().nextInt(200000)
    cache(Find(SoftID(rnd))) should be (Found(
      SoftID(rnd),
      SoftInstance(rnd, s"$rnd'th element", None)))
  }

  "endure 100.000 replace" in {
    val batch = (200001 to 300000).map(i => (SoftID(i).asInstanceOf[ID], SoftInstance(i, s"$i item", None))).toMap
    cache(ReplaceAll(batch)) should be (AllReplaced(batch))

    cache(Count) should be (Counted(100000))

    val rnd = new Random().nextInt(100000) + 200000
    cache(Find(SoftID(rnd))) should be (Found(
      SoftID(rnd),
      SoftInstance(rnd, s"$rnd item", None)))
  }

  "clear repository" in {
    cache(Count) should be (Counted(100000))
    cache(DeleteAll) should be (AllDeleted)
    cache(Count) should be (Counted(0))
  }
}
