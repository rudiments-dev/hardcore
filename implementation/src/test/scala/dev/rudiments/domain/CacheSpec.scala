package dev.rudiments.domain

import dev.rudiments.data.ReadOnly._
import dev.rudiments.data.CRUD._
import dev.rudiments.data.Batch._
import dev.rudiments.hardcore.http.query.PassAllQuery
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class CacheSpec extends WordSpec with Matchers {
  private case class Example(
    id: Long,
    name: String,
    comment: Option[String] = None
  ) extends DTO

  private implicit val domain: Domain = Domain()
  private implicit val t: Spec = domain.makeFromScala[Spec, Example]
  private val cache: Cache = new Cache
  private val sample = Instance(t, Seq(42L, "sample", None))
  private val id: ID = ID(Seq(42L))

  "no element by ID" in {
    cache(Count()) should be (Counted(0))
    cache(Find(id)) should be (NotFound(id))
  }

  "put item into repository" in {
    cache(Count()) should be (Counted(0))
    cache(Create(id, sample)) should be (Created(id, sample))
    cache(Count()) should be (Counted(1))
    cache(Find(id)) should be (Found(id, sample))
  }

  "update item in repository" in {
    cache(Update(id, Instance(t, Seq(42L, "sample", Some("changes"))))) should be (
      Updated(
        id,
        Instance(t, Seq(42L, "sample", None)),
        Instance(t, Seq(42L, "sample", Some("changes")))))
    cache(Count()) should be (Counted(1))
    cache(Find(id)) should be (Found(id, Instance(t, Seq(42L, "sample", Some("changes")))))
  }

  "multiple inserts with same ID causes exception" in {
    cache(Count()) should be (Counted(1))
    cache(Create(id, sample)) should be (AlreadyExists(id, Instance(t, Seq(42L, "sample", Some("changes")))))
  }

  "delete item from repository" in {
    cache(Count()) should be (Counted(1))
    cache(Delete(id)) should be (Deleted(id, Instance(t, Seq(42L, "sample", Some("changes")))))
    cache(Count()) should be (Counted(0))
    cache(Find(id)) should be (NotFound(id))
  }

  "endure 100.000 records" in {
    (1 to 100000)
      .map(i => Instance(t, Seq(i.toLong, s"$i'th element", None)))
      .foreach(s => cache(Create(ID(Seq(s.extract[Long]("id"))), s)))

    cache(Count()) should be (Counted(100000))

    val rnd = new Random().nextInt(100000).toLong
    cache(Find(ID(Seq(rnd)))) should be (Found(ID(Seq(rnd)), Instance(t, Seq(rnd, s"$rnd'th element", None))))
  }

  "endure 100.000 batch" in {
    val batch = (100001 to 200000).map(i => (ID(Seq(i.toLong)), Instance(t, Seq(i.toLong, s"$i'th element", None)))).toMap
    cache(CreateAll(batch)) should be (Commit(batch.map { case (k, v) => k -> Created(k, v) }))

    cache(Count()) should be (Counted(200000))

    val rnd = new Random().nextInt(200000).toLong
    cache(Find(ID(Seq(rnd)))) should be (Found(
      ID(Seq(rnd)),
      Instance(t, Seq(rnd, s"$rnd'th element", None))))
  }

  "endure 100.000 replace" in {
    val batch = (200001 to 300000).map(i => (ID(Seq(i.toLong)), Instance(t, Seq(i.toLong, s"$i item", None)))).toMap
    val deleting = cache(FindAll(PassAllQuery(t))).asInstanceOf[FoundAll].values.map { it =>
      val k = ID(Seq(it.extract[Long]("id")))
      k -> Deleted(k, it)
    }.toMap
    cache(ReplaceAll(batch)) should be (Commit(
      deleting ++ batch.map { case (k, v) => k -> Created(k, v) }
    ))

    cache(Count()) should be (Counted(100000))

    val rnd = new Random().nextInt(100000).toLong + 200000L
    cache(Find(ID(Seq(rnd)))) should be (Found(
      ID(Seq(rnd)),
      Instance(t, Seq(rnd, s"$rnd item", None))))
  }

  "clear repository" in {
    cache(Count()) should be (Counted(100000))
    val deleting = cache(FindAll(PassAllQuery(t))).asInstanceOf[FoundAll].values.map { it =>
      val k = ID(Seq(it.extract[Long]("id")))
      k -> Deleted(k, it)
    }.toMap
    cache(DeleteAll()) should be (Commit(deleting))
    cache(Count()) should be (Counted(0))
  }
}
