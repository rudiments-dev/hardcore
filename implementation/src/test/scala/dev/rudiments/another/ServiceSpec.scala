package dev.rudiments.another

import dev.rudiments.another.hardcore._
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class ServiceSpec extends AnyWordSpec with Matchers {
  case class Example(
    id: Long,
    name: String,
    comment: Option[String] = None
  )

  var txIn = 0
  var txOut = 0
  case class Counter(c: Int = txIn + 1) extends LogTx {
    txIn += 1
  }
  
  private val id = ID[Example](Seq(42L))
  private val sample = Example(42L, "sample", None)
  private val newID = ID[Example](Seq(24L))

  private val pipeline = new Pipeline[In, In, Counter] ({ rq =>(rq, Counter())})
  private val state = new State[Example]()
  private val drainage = new Drainage[Out, Counter, Out]({ (out, tx) => txOut = tx.c; out})
  private val f = new Service(pipeline, state, drainage)

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
    txIn should be (0)
    txOut should be (0)
  }

  "no element by ID" in {
    f(Count(All)) should be (Counted(0))
    txIn should be (1)
    txOut should be (1)
    f(Find(id)) should be (NotFound(id))
    txIn should be (2)
    txOut should be (2)
  }

  "put item into repository" in {
    f(Count(All)) should be (Counted(0))
    f(Create(id, sample)) should be (Created(id, sample))
    f(Count(All)) should be (Counted(1))
    f(Find(id)) should be (Found(id, sample))

    txIn should be (6)
    txOut should be (6)
  }

  "update item in repository" in {
    f(Update(id, Example(42L, "sample", Some("changes")))) should be (
      Updated(
        id,
        Example(42L, "sample", None),
        Example(42L, "sample", Some("changes"))))
    f(Count(All)) should be (Counted(1))
    f(Find(id)) should be (Found(id, Example(42L, "sample", Some("changes"))))

    txIn should be (9)
    txOut should be (9)
  }

  "move item in repository" in {
    f(Move(
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
    f(Count(All)) should be (Counted(1))
    f(Find(newID)) should be (Found(newID, Example(24L, "sample", Some("changes"))))

    txIn should be (12)
    txOut should be (12)
  }

  "multiple inserts with same ID causes exception" in {
    f(Count(All)) should be (Counted(1))
    f(Create(newID, sample)) should be (AlreadyExists(newID, Example(24L, "sample", Some("changes"))))

    txIn should be (14)
    txOut should be (14)
  }

  "delete item from repository" in {
    f(Count(All)) should be (Counted(1))
    f(Delete(newID)) should be (Deleted(newID, Example(24L, "sample", Some("changes"))))
    f(Count(All)) should be (Counted(0))
    f(Find(newID)) should be (NotFound(newID))

    txIn should be (18)
    txOut should be (18)
  }

  "endure 100.000 records" in {
    (1 to 100000)
      .map(i => Example(i.toLong, s"$i'th element", None))
      .foreach(s => f(Create(ID(Seq(s.id)), s)))

    f(Count(All)) should be (Counted(100000))

    val rnd = new Random().nextInt(100000).toLong
    f(Find(ID(Seq(rnd)))) should be (Found(ID(Seq(rnd)), Example(rnd, s"$rnd'th element", None)))

    txIn should be (100020)
    txOut should be (100020)
  }

  "endure 100.000 batch" in {
    val batch = (100001 to 200000).map(i => ID[Example](Seq(i.toLong)) -> Example(i.toLong, s"$i'th element", None)).toMap
    f(CreateAll(batch)) should be (Commit(batch.map { case (k, v) => k -> Created(k, v) }))

    f(Count(All)) should be (Counted(200000))

    val rnd = new Random().nextInt(200000).toLong
    f(Find(ID(Seq(rnd)))) should be (Found(
      ID(Seq(rnd)),
      Example(rnd, s"$rnd'th element", None)))

    txIn should be (100023)
    txOut should be (100023)
  }

  "endure 100.000 replace" in {
    val batch = (200001 to 300000).map(i => (ID[Example](Seq(i.toLong)), Example(i.toLong, s"$i item", None))).toMap
    val deleting = f(FindAll(All)).asInstanceOf[FoundAll[Example]].content.values.map { it =>
      val k = ID[Example](Seq(it.id))
      k -> Deleted(k, it)
    }.toMap
    f(ReplaceAll[Example](batch)) should be (Commit(
      deleting ++ batch.map { case (k: ID[Example], v) => k -> Created(k, v) }
    ))

    f(Count(All)) should be (Counted(100000))

    val rnd = new Random().nextInt(100000).toLong + 200000L
    f(Find(ID(Seq(rnd)))) should be (Found(
      ID(Seq(rnd)),
      Example(rnd, s"$rnd item", None)))

    txIn should be (100027)
    txOut should be (100027)
  }

  "clear repository" in {
    f(Count(All)) should be (Counted(100000))
    val deleting = f(FindAll(All)).asInstanceOf[FoundAll[Example]].content.values.map { it =>
      val k = ID[Example](Seq(it.id))
      k -> Deleted(k, it)
    }.toMap
    f(DeleteUsing(All)) should be (Commit(deleting))
    f(Count(All)) should be (Counted(0))

    txIn should be (100031)
    txOut should be (100031)
  }
}
