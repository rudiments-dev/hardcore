package dev.rudiments.data

import dev.rudiments.domain._
import dev.rudiments.hardcore.All
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class SoftCacheSpec extends AnyWordSpec with Matchers {
  private case class Example(
    id: Long,
    name: String,
    comment: Option[String] = None
  ) extends DTO

  private implicit val domain: Domain = new Domain
  private implicit val t: Spec = domain.makeFromScala[Spec, Example]
  private val state: State = new State
  private val sample = Instance(t, Seq(42L, "sample", None))
  private val id: ID = ID(Seq(42L))
  private val newID: ID = ID(Seq(24L))

  "no element by ID" in {
    state(Count(All)) should be (Counted(0))
    state(Find(id)) should be (NotFound(id))
  }

  "put item into repository" in {
    state(Count(All)) should be (Counted(0))
    state(Create(id, sample)) should be (Created(id, sample))
    state(Count(All)) should be (Counted(1))
    state(Find(id)) should be (Found(id, sample))
  }

  "update item in repository" in {
    state(Update(id, Instance(t, Seq(42L, "sample", Some("changes"))))) should be (
      Updated(
        id,
        Instance(t, Seq(42L, "sample", None)),
        Instance(t, Seq(42L, "sample", Some("changes")))))
    state(Count(All)) should be (Counted(1))
    state(Find(id)) should be (Found(id, Instance(t, Seq(42L, "sample", Some("changes")))))
  }

  "move item in repository" in {
    state(Move(
      id,
      newID,
      Instance(t, Seq(24L, "sample", Some("changes")))
    )) should be (
      Moved(
        id,
        Instance(t, Seq(42L, "sample", Some("changes"))),
        newID,
        Instance(t, Seq(24L, "sample", Some("changes"))),
      ))
    state(Count(All)) should be (Counted(1))
    state(Find(newID)) should be (Found(newID, Instance(t, Seq(24L, "sample", Some("changes")))))
  }

  "multiple inserts with same ID causes exception" in {

    state(Count(All)) should be (Counted(1))
    state(Create(newID, sample)) should be (AlreadyExists(newID, Instance(t, Seq(24L, "sample", Some("changes")))))
  }

  "delete item from repository" in {
    state(Count(All)) should be (Counted(1))
    state(Delete(newID)) should be (Deleted(newID, Instance(t, Seq(24L, "sample", Some("changes")))))
    state(Count(All)) should be (Counted(0))
    state(Find(newID)) should be (NotFound(newID))
  }

  "endure 100.000 records" in {
    (1 to 100000)
      .map(i => Instance(t, Seq(i.toLong, s"$i'th element", None)))
      .foreach(s => state(Create(ID(Seq(s.extract[Long]("id"))), s)))

    state(Count(All)) should be (Counted(100000))

    val rnd = new Random().nextInt(100000).toLong
    state(Find(ID(Seq(rnd)))) should be (Found(ID(Seq(rnd)), Instance(t, Seq(rnd, s"$rnd'th element", None))))
  }

  "endure 100.000 batch" in {
    val batch = (100001 to 200000).map(i => ID(Seq(i.toLong)) -> Instance(t, Seq(i.toLong, s"$i'th element", None))).toMap
    state(CreateAll(batch)) should be (Commit(batch.map { case (k, v) => k -> Created(k, v) }))

    state(Count(All)) should be (Counted(200000))

    val rnd = new Random().nextInt(200000).toLong
    state(Find(ID(Seq(rnd)))) should be (Found(
      ID(Seq(rnd)),
      Instance(t, Seq(rnd, s"$rnd'th element", None))))
  }

  "endure 100.000 replace" in {
    val batch = (200001 to 300000).map(i => (ID(Seq(i.toLong)), Instance(t, Seq(i.toLong, s"$i item", None)))).toMap
    val deleting = state(FindAll(All)).asInstanceOf[FoundAll].content.values.map { it =>
      val k = ID(Seq(it.extract[Long]("id")))
      k -> Deleted(k, it)
    }.toMap
    state(ReplaceAll(batch)) should be (Commit(
      deleting ++ batch.map { case (k, v) => k -> Created(k, v) }
    ))

    state(Count(All)) should be (Counted(100000))

    val rnd = new Random().nextInt(100000).toLong + 200000L
    state(Find(ID(Seq(rnd)))) should be (Found(
      ID(Seq(rnd)),
      Instance(t, Seq(rnd, s"$rnd item", None))))
  }

  "clear repository" in {
    state(Count(All)) should be (Counted(100000))
    val deleting = state(FindAll(All)).asInstanceOf[FoundAll].content.values.map { it =>
      val k = ID(Seq(it.extract[Long]("id")))
      k -> Deleted(k, it)
    }.toMap
    state(DeleteUsing(All)) should be (Commit(deleting))
    state(Count(All)) should be (Counted(0))
  }
}
