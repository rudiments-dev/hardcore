package dev.rudiments.memory

import dev.rudiments.data._
import dev.rudiments.data.DataEvent
import dev.rudiments.domain.{DTO, Domain, ID, Instance, Spec}
import dev.rudiments.hardcore.All
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

  private implicit val domain: Domain = new Domain
  private implicit val t: Spec = domain.makeFromScala[Spec, Example]
  private val store: Memory[DataEvent] = new Memory(new State)
  private val sample = Instance(t, Seq(42L, "sample", None))
  private val id: ID = ID(Seq(42L))
  private val newId: ID = ID(Seq(24L))
  private val newSample = Instance(t, Seq(24L, "sample", Some("changes")))

  "no element by ID" in {
    store(Count(All)) should be (Counted(0))
    store.of(Count(All)) should be (Counted(0))
    store.story.size should be (0)
    store.conclusion.size should be (0)
    store(Find(id)) should be (NotFound(id))
  }

  "put item into store" in {
    store(Count(All)) should be (Counted(0))
    store(Create(id, sample)) should be (Created(id, sample))

    store(Count(All)) should be (Counted(1))
    store.of(Count(All)) should be (Counted(1))
    store.story.size should be (1)
    store.conclusion.size should be (1)

    store(Find(id)) should be (Found(id, sample))
    store.of(Find(id)) should be (Found(id, sample))
    store.story.last should be (Create(id, sample) -> Created(id, sample))
    store.conclusion(id) should be (Created(id, sample))
  }

  "update item in store" in {
    store(Update(id, Instance(t, Seq(42L, "sample", Some("changes"))))) should be (
      Updated(
        id,
        Instance(t, Seq(42L, "sample", None)),
        Instance(t, Seq(42L, "sample", Some("changes")))))

    store(Count(All)) should be (Counted(1))
    store.of(Count(All)) should be (Counted(1))
    store.story.size should be (2)
    store.conclusion.size should be (1)

    store(Find(id)) should be (Found(id, Instance(t, Seq(42L, "sample", Some("changes")))))
    store.of(Find(id)) should be (Found(id, Instance(t, Seq(42L, "sample", Some("changes")))))
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

  "move item in store" in {
    store(Move(id, newId, newSample)) should be (
      Moved(
        id,
        Instance(t, Seq(42L, "sample", Some("changes"))),
        newId,
        newSample
      )
    )

    store(Count(All)) should be (Counted(1))
    store.of(Count(All)) should be (Counted(1))
    store.story.size should be (3)
    store.conclusion.size should be (2)

    store(Find(newId)) should be (Found(newId, newSample))
    store.of(Find(newId)) should be (Found(newId, newSample))
    store.story.last should be (
      Move(id, newId, newSample) -> Moved(
        id,
        Instance(t, Seq(42L, "sample", Some("changes"))),
        newId,
        newSample
      )
    )
    store.conclusion(newId) should be (Moved(
      id,
      Instance(t, Seq(42L, "sample", Some("changes"))),
      newId,
      newSample
    ))
  }

  "multiple inserts with same ID causes exception" in {
    store(Count(All)) should be (Counted(1))
    store(Create(newId, sample)) should be (AlreadyExists(newId, newSample))
  }

  "delete item from store" in {
    store(Count(All)) should be (Counted(1))
    store.of(Count(All)) should be (Counted(1))
    store.story.size should be (3)
    store.conclusion.size should be (2)

    store(Delete(newId)) should be (Deleted(newId, newSample))
    store.story.last should be (Delete(newId) -> Deleted(newId, newSample))
    store.conclusion(newId) should be (Deleted(newId, newSample))

    store(Count(All)) should be (Counted(0))
    store.of(Count(All)) should be (Counted(0))
    store.story.size should be (4)
    store.conclusion.size should be (2)

    store(Find(newId)) should be (NotFound(newId))
  }

  "endure 100.000 records" in {
    (1 to 100000)
      .map(i => Instance(t, Seq(i.toLong, s"$i'th element", None)))
      .foreach(s => store(Create(ID(Seq(s.extract[Long]("id"))), s)))

    store(Count(All)) should be (Counted(100000))
    store.of(Count(All)) should be (Counted(100000))
    store.story.size should be (100004)
    store.conclusion.size should be (100000)

    val rnd = new Random().nextInt(100000).toLong
    store(Find(ID(Seq(rnd)))) should be (Found(ID(Seq(rnd)), Instance(t, Seq(rnd, s"$rnd'th element", None))))
  }

  "endure 100.000 batch" in {
    val batch = (100001 to 200000).map(i => (ID(Seq(i.toLong)), Instance(t, Seq(i.toLong, s"$i'th element", None)))).toMap
    store(CreateAll(batch)) should be (Commit(batch.map { case (k, v) => k -> Created(k, v) }))

    store(Count(All)) should be (Counted(200000))
    store.of(Count(All)) should be (Counted(200000))
    store.story.size should be (200004)
    store.conclusion.size should be (200000)

    val rnd = new Random().nextInt(200000).toLong
    store(Find(ID(Seq(rnd)))) should be (Found(
      ID(Seq(rnd)),
      Instance(t, Seq(rnd, s"$rnd'th element", None))))
  }

  "endure 100.000 replace" in {
    val batch = (200001 to 300000).map(i => (ID(Seq(i.toLong)), Instance(t, Seq(i.toLong, s"$i item", None)))).toMap
    val deleting = store(FindAll(All)).asInstanceOf[FoundAll].values.map { it =>
      val k = ID(Seq(it.extract[Long]("id")))
      k -> Deleted(k, it)
    }.toMap
    store(ReplaceAll(batch)) should be (Commit(
      deleting ++ batch.map { case (k, v) => k -> Created(k, v) }
    ))

    store(Count(All)) should be (Counted(100000))
    store.of(Count(All)) should be (Counted(100000))
    store.story.size should be (500004)
    store.conclusion.size should be (300000)

    val rnd = new Random().nextInt(100000).toLong + 200000L
    store(Find(ID(Seq(rnd)))) should be (Found(
      ID(Seq(rnd)),
      Instance(t, Seq(rnd, s"$rnd item", None))))
  }

  "clear repository" in {
    store(Count(All)) should be (Counted(100000))
    val deleting = store(FindAll(All)).asInstanceOf[FoundAll].values.map { it =>
      val k = ID(Seq(it.extract[Long]("id")))
      k -> Deleted(k, it)
    }.toMap
    store(DeleteUsing(All)) should be (Commit(deleting))
    store(Count(All)) should be (Counted(0))
    store.of(Count(All)) should be (Counted(0))
    store.story.size should be (600004)
    store.conclusion.size should be (300000)
  }
}
