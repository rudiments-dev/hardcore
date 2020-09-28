package dev.rudiments.memory

import dev.rudiments.data.ReadOnly._
import dev.rudiments.data.CRUD._
import dev.rudiments.data.Batch._
import dev.rudiments.domain.{DTO, Domain, ID, Instance, Spec}
import dev.rudiments.hardcore.http.query.PassAllQuery
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class MemorySpec extends WordSpec with Matchers {
  private case class Example(
    id: Long,
    name: String,
    comment: Option[String] = None
  ) extends DTO

  private implicit val domain: Domain = Domain()
  private implicit val t: Spec = domain.makeFromScala[Spec, Example]
  private val store: Memory = new Memory
  private val sample = Instance(t, Seq(42L, "sample", None))
  private val id: ID = ID(Seq(42L))

  "no element by ID" in {
    store(Count()).merge should be (Counted(0))
    store.state.size should be (0)
    store.story.size should be (0)
    store.conclusion.size should be (0)
    store(Find(id)).merge should be (NotFound(id))
  }

  "put item into store" in {
    store(Count()).merge should be (Counted(0))
    store(Create(id, sample)).merge should be (Created(id, sample))

    store(Count()).merge should be (Counted(1))
    store.state.size should be (1)
    store.story.size should be (1)
    store.conclusion.size should be (1)

    store(Find(id)).merge should be (Found(id, sample))
    store.state(id) should be (sample)
    store.story.last should be (Create(id, sample) -> Created(id, sample))
    store.conclusion(id) should be (Created(id, sample))
  }

  "update item in store" in {
    store(Update(id, Instance(t, Seq(42L, "sample", Some("changes"))))).merge should be (
      Updated(
        id,
        Instance(t, Seq(42L, "sample", None)),
        Instance(t, Seq(42L, "sample", Some("changes")))))

    store(Count()).merge should be (Counted(1))
    store.state.size should be (1)
    store.story.size should be (2)
    store.conclusion.size should be (1)

    store(Find(id)).merge should be (Found(id, Instance(t, Seq(42L, "sample", Some("changes")))))
    store.state(id) should be (Instance(t, Seq(42L, "sample", Some("changes"))))
    store.story.last should be (
      Update(id, Instance(t, Seq(42L, "sample", Some("changes")))) -> Updated(
        id,
        Instance(t, Seq(42L, "sample", None)),
        Instance(t, Seq(42L, "sample", Some("changes")))
      )
    )
    store.conclusion(id) should be (Updated(
      id,
      Instance(t, Seq(42L, "sample", None)),
      Instance(t, Seq(42L, "sample", Some("changes")))
    ))
  }

  "multiple inserts with same ID causes exception" in {
    store(Count()).merge should be (Counted(1))
    store(Create(id, sample)).merge should be (AlreadyExists(id, Instance(t, Seq(42L, "sample", Some("changes")))))
  }

  "delete item from store" in {
    store(Count()).merge should be (Counted(1))
    store.state.size should be (1)
    store.story.size should be (2)
    store.conclusion.size should be (1)

    store(Delete(id)).merge should be (Deleted(id, Instance(t, Seq(42L, "sample", Some("changes")))))
    store.state.get(id) should be (None)
    store.story.last should be (Delete(id) -> Deleted(id, Instance(t, Seq(42L, "sample", Some("changes")))))
    store.conclusion(id) should be (Deleted(id, Instance(t, Seq(42L, "sample", Some("changes")))))

    store(Count()).merge should be (Counted(0))
    store.state.size should be (0)
    store.story.size should be (3)
    store.conclusion.size should be (1)

    store(Find(id)).merge should be (NotFound(id))
  }

  "endure 100.000 records" in {
    (1 to 100000)
      .map(i => Instance(t, Seq(i.toLong, s"$i'th element", None)))
      .foreach(s => store(Create(ID(Seq(s.extract[Long]("id"))), s)))

    store(Count()).merge should be (Counted(100000))
    store.state.size should be (100000)
    store.story.size should be (100003)
    store.conclusion.size should be (100000)

    val rnd = new Random().nextInt(100000).toLong
    store(Find(ID(Seq(rnd)))).merge should be (Found(ID(Seq(rnd)), Instance(t, Seq(rnd, s"$rnd'th element", None))))
  }

  "endure 100.000 batch" in {
    val batch = (100001 to 200000).map(i => (ID(Seq(i.toLong)), Instance(t, Seq(i.toLong, s"$i'th element", None)))).toMap
    store(CreateAll(batch)).merge should be (Commit(batch.map { case (k, v) => k -> Created(k, v) }))

    store(Count()).merge should be (Counted(200000))
    store.state.size should be (200000)
    store.story.size should be (200003)
    store.conclusion.size should be (200000)

    val rnd = new Random().nextInt(200000).toLong
    store(Find(ID(Seq(rnd)))).merge should be (Found(
      ID(Seq(rnd)),
      Instance(t, Seq(rnd, s"$rnd'th element", None))))
  }

  "endure 100.000 replace" in {
    val batch = (200001 to 300000).map(i => (ID(Seq(i.toLong)), Instance(t, Seq(i.toLong, s"$i item", None)))).toMap
    val deleting = store(FindAll(PassAllQuery(t))).merge.asInstanceOf[FoundAll].values.map { it =>
      val k = ID(Seq(it.extract[Long]("id")))
      k -> Deleted(k, it)
    }.toMap
    store(ReplaceAll(batch)).merge should be (Commit(
      deleting ++ batch.map { case (k, v) => k -> Created(k, v) }
    ))

    store(Count()).merge should be (Counted(100000))
    store.state.size should be (100000)
    store.story.size should be (500003)
    store.conclusion.size should be (300000)

    val rnd = new Random().nextInt(100000).toLong + 200000L
    store(Find(ID(Seq(rnd)))).merge should be (Found(
      ID(Seq(rnd)),
      Instance(t, Seq(rnd, s"$rnd item", None))))
  }

  "clear repository" in {
    store(Count()).merge should be (Counted(100000))
    val deleting = store(FindAll(PassAllQuery(t))).merge.asInstanceOf[FoundAll].values.map { it =>
      val k = ID(Seq(it.extract[Long]("id")))
      k -> Deleted(k, it)
    }.toMap
    store(DeleteAll()).merge should be (Commit(deleting))
    store(Count()).merge should be (Counted(0))
    store.state.size should be (0)
    store.story.size should be (600003)
    store.conclusion.size should be (300000)
  }
}