package dev.rudiments.data

import dev.rudiments.data.Batch._
import dev.rudiments.data.CRUD._
import dev.rudiments.data.ReadOnly._
import dev.rudiments.types._
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

  private implicit val typeSystem: TypeSystem = new TypeSystem()
  private implicit val t: Type = typeSystem.asType[Example]
  private val cache: SoftCache = new SoftCache
  private val sample = Instance(42L, "sample", None)
  private val id: ID = sample.extractID("id")

  "no element by ID" in {
    cache(Count()).merge should be (Counted(0))
    cache(Find(id)).merge should be (NotFound(id))
  }

  "put item into repository" in {
    cache(Count()).merge should be (Counted(0))
    cache(Create(id, sample)).merge should be (Created(id, sample))
    cache(Count()).merge should be (Counted(1))
    cache(Find(id)).merge should be (Found(id, sample))
  }

  "update item in repository" in {
    cache(Update(id, Instance(42L, "sample", Some("changes")))).merge should be (
      Updated(
        id,
        Instance(42L, "sample", None),
        Instance(42L, "sample", Some("changes"))))
    cache(Count()).merge should be (Counted(1))
    cache(Find(id)).merge should be (Found(id, Instance(42L, "sample", Some("changes"))))
  }

  "multiple inserts with same ID causes exception" in {
    cache(Count()).merge should be (Counted(1))
    cache(Create(id, sample)).merge should be (AlreadyExists(id, Instance(42L, "sample", Some("changes"))))
  }

  "delete item from repository" in {
    cache(Count()).merge should be (Counted(1))
    cache(Delete(id)).merge should be (Deleted(id, Instance(42L, "sample", Some("changes"))))
    cache(Count()).merge should be (Counted(0))
    cache(Find(id)).merge should be (NotFound(id))
  }

  "endure 100.000 records" in {
    (1 to 100000)
      .map(i => Instance(i.toLong, s"$i'th element", None))
      .foreach(s => cache(Create(s.extractID("id"), s)))

    cache(Count()).merge should be (Counted(100000))

    val rnd = new Random().nextInt(100000).toLong
    cache(Find(ID(rnd))).merge should be (Found(ID(rnd), Instance(rnd, s"$rnd'th element", None)))
  }

  "endure 100.000 batch" in {
    val batch = (100001 to 200000).map(i => (ID(i.toLong).asInstanceOf[ID], Instance(i.toLong, s"$i'th element", None))).toMap
    cache(CreateAll(batch)).merge should be (AllCreated(batch))

    cache(Count()).merge should be (Counted(200000))

    val rnd = new Random().nextInt(200000).toLong
    cache(Find(ID(rnd))).merge should be (Found(
      ID(rnd),
      Instance(rnd, s"$rnd'th element", None)))
  }

  "endure 100.000 replace" in {
    val batch = (200001 to 300000).map(i => (ID(i.toLong).asInstanceOf[ID], Instance(i.toLong, s"$i item", None))).toMap
    cache(ReplaceAll(batch)).merge should be (AllReplaced(batch))

    cache(Count()).merge should be (Counted(100000))

    val rnd = new Random().nextInt(100000).toLong + 200000L
    cache(Find(ID(rnd))).merge should be (Found(
      ID(rnd),
      Instance(rnd, s"$rnd item", None)))
  }

  "clear repository" in {
    cache(Count()).merge should be (Counted(100000))
    cache(DeleteAll()).merge should be (AllDeleted())
    cache(Count()).merge should be (Counted(0))
  }
}
