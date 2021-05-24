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

  val store = new Store[Example, Example]()

  "signature of store" in {
    store.skills.map(s => s._1 -> s._2.outcomes) should be (Map(
      ID[In, String]("dev.rudiments.hardcore.Create") -> Set(ID[In, String]("dev.rudiments.hardcore.Created")),
      ID[In, String]("dev.rudiments.hardcore.Read") -> Set(ID[In, String]("dev.rudiments.hardcore.Readen"), ID[In, String]("dev.rudiments.hardcore.NotFound")),
      ID[In, String]("dev.rudiments.hardcore.Update") -> Set(ID[In, String]("dev.rudiments.hardcore.Updated")),
      ID[In, String]("dev.rudiments.hardcore.Upsert") -> Set(ID[In, String]("dev.rudiments.hardcore.Updated")),
      ID[In, String]("dev.rudiments.hardcore.Delete") -> Set(ID[In, String]("dev.rudiments.hardcore.Deleted")),

      ID[In, String]("dev.rudiments.hardcore.Copy") -> Set(ID[In, String]("dev.rudiments.hardcore.Copied")),
      ID[In, String]("dev.rudiments.hardcore.Move") -> Set(ID[In, String]("dev.rudiments.hardcore.Moved")),

      ID[In, String]("dev.rudiments.hardcore.Count") -> Set(ID[In, String]("dev.rudiments.hardcore.Counted")),
      ID[In, String]("dev.rudiments.hardcore.Find") -> Set(ID[In, String]("dev.rudiments.hardcore.Found")),

      ID[In, String]("dev.rudiments.hardcore.Reconcile") -> Set(ID[In, String]("dev.rudiments.hardcore.Commit")),
      ID[In, String]("dev.rudiments.hardcore.CreateAll") -> Set(ID[In, String]("dev.rudiments.hardcore.Commit")),
      ID[In, String]("dev.rudiments.hardcore.DeleteUsing") -> Set(ID[In, String]("dev.rudiments.hardcore.Commit")),
      ID[In, String]("dev.rudiments.hardcore.ReplaceAll") -> Set(ID[In, String]("dev.rudiments.hardcore.Commit")),
    ))
  }

  "no element by ID in store" in {
    store(Count(All)) should be (Counted(0))
    store(Read(id)) should be (NotFound(id))
  }

  "put item into store" in {
    store(Count(All)) should be (Counted(0))
    store(Create(id, sample)) should be (Created(id, sample))
    store(Count(All)) should be (Counted(1))
    store(Read(id)) should be (Readen(id, sample))
  }

  "update item in store" in {
    store(Update(id, Example(42L, "sample", Some("changes")))) should be (
      Updated(
        id,
        Example(42L, "sample", None),
        Example(42L, "sample", Some("changes"))))
    store(Count(All)) should be (Counted(1))
    store(Read(id)) should be (Readen(id, Example(42L, "sample", Some("changes"))))
  }

  "move item in store" in {
    store(Move(
      id,
      newID,
      Example(24L, "sample", Some("changes"))
    )) should be (
      Moved(
        id,
        Example(42L, "sample", Some("changes")),
        newID,
        Example(24L, "sample", Some("changes"))
      ))
    store(Count(All)) should be (Counted(1))
    store(Read(newID)) should be (Readen(newID, Example(24L, "sample", Some("changes"))))
  }

  "copy item in store" in {
    store(Copy(
      newID,
      id,
      Example(42L, "sample", Some("changes"))
    )) should be (
      Copied(
        newID,
        Example(24L, "sample", Some("changes")),
        id,
        Example(42L, "sample", Some("changes"))
      ))
    store(Count(All)) should be (Counted(2))
    store(Read(newID)) should be (Readen(newID, Example(24L, "sample", Some("changes"))))
    store(Read(id)) should be (Readen(id, Example(42L, "sample", Some("changes"))))
  }

  "multiple inserts with same ID causes exception" in {
    store(Count(All)) should be (Counted(2))
    store(Create(newID, sample)) should be (AlreadyExists(newID, Example(24L, "sample", Some("changes"))))
  }

  "delete item from store" in {
    store(Count(All)) should be (Counted(2))
    store(Delete(newID)) should be (Deleted(newID, Example(24L, "sample", Some("changes"))))
    store(Count(All)) should be (Counted(1))
    store(Read(newID)) should be (NotFound(newID))
    store(Delete(id)) should be (Deleted(id, Example(42L, "sample", Some("changes"))))
    store(Count(All)) should be (Counted(0))
    store(Read(id)) should be (NotFound(id))
  }

  "endure 100.000 Creates into store" in {
    (1 to 100000)
      .map(i => Example(i.toLong, s"$i'th element", None))
      .foreach(s => store(Create(ID(s.id), s)))

    store(Count(All)) should be (Counted(100000))

    val rnd = new Random().nextInt(100000).toLong
    store(Read(ID(rnd))) should be (Readen(ID(rnd), Example(rnd, s"$rnd'th element", None)))
  }

  "endure 100.000 CreateAll into store" in {
    val batch = (100001 to 200000).map(i => ID[Example, Long](i.toLong) -> Example(i.toLong, s"$i'th element", None)).toMap
    store(CreateAll(batch)) should be (Commit(batch.map { case (k, v) => k -> Created(k, v) }))

    store(Count(All)) should be (Counted(200000))

    val rnd = new Random().nextInt(200000).toLong
    store(Read(ID(rnd))) should be (Readen(
      ID(rnd),
      Example(rnd, s"$rnd'th element", None)))
  }

  "endure 100.000 Replace in store" in {
    val batch = (200001 to 300000).map(i => (ID[Example, Long](i.toLong), Example(i.toLong, s"$i item", None))).toMap
    val deleting = store(Find(All)).asInstanceOf[Found[Example, Example]].content.values.map { it =>
      val k = ID[Example, Long](it.id)
      k -> Deleted(k, it)
    }.toMap
    store(ReplaceAll[Example, Example](batch)) should be (Commit(
      deleting ++ batch.map { case (k: ID[Example], v) => k -> Created[Example, Example](k, v) }
    ))

    store(Count(All)) should be (Counted(100000))

    val rnd = new Random().nextInt(100000).toLong + 200000L
    store(Read(ID(rnd))) should be (Readen(
      ID(rnd),
      Example(rnd, s"$rnd item", None)))
  }

  "clear store" in {
    store(Count(All)) should be (Counted(100000))
    val deleting = store(Find(All)).asInstanceOf[Found[Example, Example]].content.values.map { it =>
      val k = ID[Example, Long](it.id)
      k -> Deleted(k, it)
    }.toMap
    store(DeleteUsing(All)) should be (Commit(deleting))
    store(Count(All)) should be (Counted(0))
  }
}
