package dev.rudiments.domain

import dev.rudiments.data._
import dev.rudiments.hardcore.{All, Equals, FieldExpression, Less, LessOrEquals, More, MoreOrEquals, ParameterExpression, TypedPredicate}
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class StateSpec extends AnyWordSpec with Matchers {
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

  "multiple inserts with same ID causes exception" in {
    state(Count(All)) should be (Counted(1))
    state(Create(id, sample)) should be (AlreadyExists(id, Instance(t, Seq(42L, "sample", Some("changes")))))
  }

  "delete item from repository" in {
    state(Count(All)) should be (Counted(1))
    state(Delete(id)) should be (Deleted(id, Instance(t, Seq(42L, "sample", Some("changes")))))
    state(Count(All)) should be (Counted(0))
    state(Find(id)) should be (NotFound(id))
  }

  "endure 100.000 records" in {
    (1 to 100000)
      .map(i => Instance(t, Seq(i.toLong, s"$i'th element", None)))
      .foreach(s => state(Create(ID(Seq(s.extract[Long]("id"))), s)))

    state(Count(All)) should be (Counted(100000))

    val rnd = new Random().nextInt(100000).toLong
    state(Find(ID(Seq(rnd)))) should be (Found(ID(Seq(rnd)), Instance(t, Seq(rnd, s"$rnd'th element", None))))
  }

  "find equals value" in {
    state(
      FindAll(
        TypedPredicate(t, Seq(Equals(FieldExpression("name"), ParameterExpression("13666'th element"))))
      )
    ) should be (FoundAll(Map(
      ID(Seq(13666L)) -> Instance(t, Seq(13666L, s"13666'th element", None))
    )))
  }

  "find more than value" in {
    state(FindAll(
        TypedPredicate(t, Seq(More(FieldExpression("id"), ParameterExpression(99999L))))
    )) should be (FoundAll(Map(
      ID(Seq(100000L)) -> Instance(t, Seq(100000L, s"100000'th element", None))
    )))
  }

  "find more or equals than value" in {
    state(FindAll(
      TypedPredicate(t, Seq(MoreOrEquals(FieldExpression("id"), ParameterExpression(99999L))))
    )) should be (FoundAll(Map(
      ID(Seq(99999L)) -> Instance(t, Seq(99999L, s"99999'th element", None)),
      ID(Seq(100000L)) -> Instance(t, Seq(100000L, s"100000'th element", None))
    )))
  }

  "find less than value" in {
    state(FindAll(
      TypedPredicate(t, Seq(Less(FieldExpression("id"), ParameterExpression(2L))))
    )) should be (FoundAll(Map(
      ID(Seq(1L)) -> Instance(t, Seq(1L, s"1'th element", None))
    )))
  }

  "find less or equals than value" in {
    state(FindAll(
      TypedPredicate(t, Seq(LessOrEquals(FieldExpression("id"), ParameterExpression(2L))))
    )) should be (FoundAll(Map(
      ID(Seq(2L)) -> Instance(t, Seq(2L, s"2'th element", None)),
      ID(Seq(1L)) -> Instance(t, Seq(1L, s"1'th element", None))
    )))
  }

  "endure 100.000 batch" in {
    val batch = (100001 to 200000).map(i => (ID(Seq(i.toLong)), Instance(t, Seq(i.toLong, s"$i'th element", None)))).toMap
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
