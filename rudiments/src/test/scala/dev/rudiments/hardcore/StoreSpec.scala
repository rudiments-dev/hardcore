package dev.rudiments.hardcore

import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class StoreSpec extends AnyWordSpec with Matchers {
  case class Example(
    id: Long,
    name: String,
    comment: Option[String] = None
  )
  
  private val id = ID[Example, Long](42L)
  private val sample = Example(42L, "sample", None)
  private val newID = ID[Example, Long](24L)

  val state = new Store[Example]()

  "signature of state" ignore {
    //TODO
  }

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
    state(Update(id, Example(42L, "sample", Some("changes")))) should be (
      Updated(
        id,
        Example(42L, "sample", None),
        Example(42L, "sample", Some("changes"))))
    state(Count(All)) should be (Counted(1))
    state(Find(id)) should be (Found(id, Example(42L, "sample", Some("changes"))))
  }

  "move item in repository" in {
    state(Move(
      id,
      newID,
      Example(24L, "sample", Some("changes"))
    )) should be (
      Moved(
        id,
        Example(42L, "sample", Some("changes")),
        newID,
        Example(24L, "sample", Some("changes")),
      ))
    state(Count(All)) should be (Counted(1))
    state(Find(newID)) should be (Found(newID, Example(24L, "sample", Some("changes"))))
  }

  "multiple inserts with same ID causes exception" in {
    state(Count(All)) should be (Counted(1))
    state(Create(newID, sample)) should be (AlreadyExists(newID, Example(24L, "sample", Some("changes"))))
  }

  "delete item from repository" in {
    state(Count(All)) should be (Counted(1))
    state(Delete(newID)) should be (Deleted(newID, Example(24L, "sample", Some("changes"))))
    state(Count(All)) should be (Counted(0))
    state(Find(newID)) should be (NotFound(newID))
  }

  "endure 100.000 records" in {
    (1 to 100000)
      .map(i => Example(i.toLong, s"$i'th element", None))
      .foreach(s => state(Create(ID(s.id), s)))

    state(Count(All)) should be (Counted(100000))

    val rnd = new Random().nextInt(100000).toLong
    state(Find(ID(rnd))) should be (Found(ID(rnd), Example(rnd, s"$rnd'th element", None)))
  }

  "endure 100.000 batch" in {
    val batch = (100001 to 200000).map(i => ID[Example, Long](i.toLong) -> Example(i.toLong, s"$i'th element", None)).toMap
    state(CreateAll(batch)) should be (Commit(batch.map { case (k, v) => k -> Created(k, v) }))

    state(Count(All)) should be (Counted(200000))

    val rnd = new Random().nextInt(200000).toLong
    state(Find(ID(rnd))) should be (Found(
      ID(rnd),
      Example(rnd, s"$rnd'th element", None)))
  }

  "endure 100.000 replace" in {
    val batch = (200001 to 300000).map(i => (ID[Example, Long](i.toLong), Example(i.toLong, s"$i item", None))).toMap
    val deleting = state(FindAll(All)).asInstanceOf[FoundAll[Example, Example]].content.values.map { it =>
      val k = ID[Example, Long](it.id)
      k -> Deleted(k, it)
    }.toMap
    state(ReplaceAll[Example, Example](batch)) should be (Commit(
      deleting ++ batch.map { case (k: ID[Example], v) => k -> Created[Example, Example](k, v) }
    ))

    state(Count(All)) should be (Counted(100000))

    val rnd = new Random().nextInt(100000).toLong + 200000L
    state(Find(ID(rnd))) should be (Found(
      ID(rnd),
      Example(rnd, s"$rnd item", None)))
  }

  "clear repository" in {
    state(Count(All)) should be (Counted(100000))
    val deleting = state(FindAll(All)).asInstanceOf[FoundAll[Example, Example]].content.values.map { it =>
      val k = ID[Example, Long](it.id)
      k -> Deleted(k, it)
    }.toMap
    state(DeleteUsing(All)) should be (Commit(deleting))
    state(Count(All)) should be (Counted(0))
  }
}
