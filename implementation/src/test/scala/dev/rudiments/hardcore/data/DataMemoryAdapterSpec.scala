package dev.rudiments.hardcore.data

import dev.rudiments.hardcore.data.Batch._
import dev.rudiments.hardcore.data.CRUD._
import dev.rudiments.hardcore.data.ReadOnly._
import dev.rudiments.hardcore.types.{DTO, HardID, HardType}
import org.junit.runner.RunWith
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.junit.JUnitRunner

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class DataMemoryAdapterSpec extends WordSpec with Matchers {
  private case class Example(
    id: Long,
    name: String,
    enum: MyEnum,
    comment: Option[String] = None
  ) extends DTO

  private implicit val t: HardType[Example] = HardType[Example]
  private val repo: DataMemoryAdapter[Example] = new DataMemoryAdapter[Example]
  private val sample = Example(42, "sample", MyEnum.Red)
  private val id: HardID[Example] = HardID(sample.id)

  "no element by ID" in {
    repo(Count[Example]()) should be (Counted(0))
    repo(Find(id)) should be (NotFound(id))
  }

  "put item into repository" in {
    repo(Count[Example]()) should be (Counted(0))
    repo(Create(id, sample)) should be (Created(id, sample))
    repo(Count[Example]()) should be (Counted(1))
    repo(Find(id)) should be (Found(id, sample))
  }

  "update item in repository" in {
    repo(Update(id, sample.copy(comment = Some("changes")))) should be (
      Updated(
        id,
        Example(42, "sample", MyEnum.Red),
        Example(42, "sample", MyEnum.Red, Some("changes"))))
    repo(Count[Example]()) should be (Counted(1))
    repo(Find(id)) should be (Found(id, Example(42, "sample", MyEnum.Red, Some("changes"))))
  }

  "multiple inserts with same ID causes exception" in {
    repo(Count[Example]()) should be (Counted(1))
    repo(Create(id, sample)) should be (AlreadyExists(id, Example(42, "sample", MyEnum.Red, Some("changes"))))
  }

  "delete item from repository" in {
    repo(Count[Example]()) should be (Counted(1))
    repo(Delete(id)) should be (Deleted(id, Example(42, "sample", MyEnum.Red, Some("changes"))))
    repo(Count[Example]()) should be (Counted(0))
    repo(Find(id)) should be (NotFound(id))
  }

  "endure 100.000 records" in {
    (1 to 100000)
      .map(i => Example(i, s"$i'th element", MyEnum.One))
      .foreach(sample => repo(Create(HardID(sample.id), sample)))

    repo(Count[Example]()) should be (Counted(100000))

    val rnd = new Random().nextInt(100000)
    repo(Find(HardID(rnd))) should be (Found(HardID(rnd), Example(rnd, s"$rnd'th element", MyEnum.One)))
  }

  "endure 100.000 batch" in {
    val batch = (100001 to 200000).map(i => (HardID[Example, Long](i), Example(i, s"$i'th element", MyEnum.Two))).toMap
    repo(CreateAll(batch)) should be (AllCreated(batch))

    repo(Count[Example]()) should be (Counted(200000))

    val rnd = new Random().nextInt(200000)
    repo(Find(HardID[Example, Long](rnd))) should be (Found(
      HardID[Example, Long](rnd),
      Example(rnd, s"$rnd'th element", if(rnd > 100000) MyEnum.Two else MyEnum.One)))
  }

  "endure 100.000 replace" in {
    val batch = (200001 to 300000).map(i => (HardID[Example, Long](i), Example(i, s"$i item", MyEnum.Red))).toMap
    repo(ReplaceAll(batch)) should be (AllReplaced(batch))

    repo(Count[Example]()) should be (Counted(100000))

    val rnd = new Random().nextInt(100000) + 200000
    repo(Find(HardID[Example, Long](rnd))) should be (Found(
      HardID[Example, Long](rnd),
      Example(rnd, s"$rnd item", MyEnum.Red)))
  }

  "clear repository" in {
    repo(Count[Example]()) should be (Counted(100000))
    repo(DeleteAll[Example]()) should be (AllDeleted[Example]())
    repo(Count[Example]()) should be (Counted(0))
  }
}
