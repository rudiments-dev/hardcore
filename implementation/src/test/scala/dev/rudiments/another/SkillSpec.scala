package dev.rudiments.another

import dev.rudiments.another.hardcore._
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class SkillSpec extends AnyWordSpec with Matchers {
  case class Example(
    id: Long,
    name: String,
    comment: Option[String] = None
  )
  
  private val id = ID[Example](Seq(42L))
  private val sample = Example(42L, "sample", None)
  private val newID = ID[Example](Seq(24L))

  val state = new State[Example]()

  "signature of state" in {
    state.signature should be (Seq(
      ID[In](Seq("dev.rudiments.another.hardcore.Count")) -> ID[Out](Seq("dev.rudiments.another.hardcore.Counted")),
      ID[In](Seq("dev.rudiments.another.hardcore.Find")) -> ID[Out](Seq("dev.rudiments.another.hardcore.Found")),
      ID[In](Seq("dev.rudiments.another.hardcore.FindAll")) -> ID[Out](Seq("dev.rudiments.another.hardcore.FoundAll")),
      ID[In](Seq("dev.rudiments.another.hardcore.Create")) -> ID[Out](Seq("dev.rudiments.another.hardcore.Created")),
      ID[In](Seq("dev.rudiments.another.hardcore.Update")) -> ID[Out](Seq("dev.rudiments.another.hardcore.Updated")),
      ID[In](Seq("dev.rudiments.another.hardcore.Move")) -> ID[Out](Seq("dev.rudiments.another.hardcore.Moved")),
      ID[In](Seq("dev.rudiments.another.hardcore.Delete")) -> ID[Out](Seq("dev.rudiments.another.hardcore.Deleted")),
      ID[In](Seq("dev.rudiments.another.hardcore.CreateAll")) -> ID[Out](Seq("dev.rudiments.another.hardcore.Commit")),
      ID[In](Seq("dev.rudiments.another.hardcore.ReplaceAll")) -> ID[Out](Seq("dev.rudiments.another.hardcore.Commit")),
      ID[In](Seq("dev.rudiments.another.hardcore.DeleteUsing")) -> ID[Out](Seq("dev.rudiments.another.hardcore.Commit")),
      ID[In](Seq("dev.rudiments.another.hardcore.Reconcile")) -> ID[Out](Seq("dev.rudiments.another.hardcore.Commit"))
    ))
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
      .foreach(s => state(Create(ID(Seq(s.id)), s)))

    state(Count(All)) should be (Counted(100000))

    val rnd = new Random().nextInt(100000).toLong
    state(Find(ID(Seq(rnd)))) should be (Found(ID(Seq(rnd)), Example(rnd, s"$rnd'th element", None)))
  }

  "endure 100.000 batch" in {
    val batch = (100001 to 200000).map(i => ID[Example](Seq(i.toLong)).asInstanceOf[Identifier] -> Example(i.toLong, s"$i'th element", None)).toMap
    state(CreateAll(batch)) should be (Commit(batch.map { case (k, v) => k -> Created(k, v) }))

    state(Count(All)) should be (Counted(200000))

    val rnd = new Random().nextInt(200000).toLong
    state(Find(ID(Seq(rnd)))) should be (Found(
      ID(Seq(rnd)),
      Example(rnd, s"$rnd'th element", None)))
  }

  "endure 100.000 replace" in {
    val batch = (200001 to 300000).map(i => (ID[Example](Seq(i.toLong)).asInstanceOf[Identifier], Example(i.toLong, s"$i item", None))).toMap
    val deleting = state(FindAll(All)).asInstanceOf[FoundAll[Example]].content.values.map { it =>
      val k = ID[Example](Seq(it.id)).asInstanceOf[Identifier]
      k -> Deleted(k, it)
    }.toMap
    state(ReplaceAll[Example](batch)) should be (Commit(
      deleting ++ batch.map { case (k: Identifier, v) => k -> Created[Example](k, v) }
    ))

    state(Count(All)) should be (Counted(100000))

    val rnd = new Random().nextInt(100000).toLong + 200000L
    state(Find(ID(Seq(rnd)))) should be (Found(
      ID(Seq(rnd)),
      Example(rnd, s"$rnd item", None)))
  }

  "clear repository" in {
    state(Count(All)) should be (Counted(100000))
    val deleting = state(FindAll(All)).asInstanceOf[FoundAll[Example]].content.values.map { it =>
      val k = ID[Example](Seq(it.id)).asInstanceOf[Identifier]
      k -> Deleted(k, it)
    }.toMap
    state(DeleteUsing(All)) should be (Commit(deleting))
    state(Count(All)) should be (Counted(0))
  }
}
