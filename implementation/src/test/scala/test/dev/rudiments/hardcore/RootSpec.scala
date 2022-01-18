package test.dev.rudiments.hardcore

import dev.rudiments.hardcore._
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner
import test.dev.rudiments.Smt

import scala.collection.mutable

@RunWith(classOf[JUnitRunner])
class RootSpec extends AnyWordSpec with Matchers {
  private implicit val space: Space = new Space()
  private val somePath: Path = ID("one")/ID(42)/ID("path")

  private val path: Path = somePath / ID("agent")

  private val id = ID("42")
  private val data = Smt(42L, "test", None).asData

  "able to navigate the path" in {
    space.add(somePath, ID("agent"), new Memory(All, All))
    path.find.state should be (mutable.SeqMap.empty[ID, Thing])
  }

  "NotFound by ID" in {
    path(Read(id)) should be (NotFound(id))
  }

  "can Create" in {
    path(Create(id, data)) should be (Created(id, data))
  }

  "can Read existing ID" in {
    path(Read(id)) should be (Readen(id, data))
  }

  private val elseData = Smt(31, "sample", Some("thing")).asData
  "can Update" in {
    path(Update(id, elseData))
  }

  "can Read updated ID" in {
    path(Read(id)) should be (Readen(id, elseData))
  }

  "can Delete" in {
    path(Delete(id)) should be (Deleted(id, elseData))
  }

  "NotFound after Delete" in {
    path(Read(id)) should be (NotFound(id)) // or Deleted(id, elseData) ?
  }

  private val wrongId = ID(666)
  "NotFound if Update with wrong ID" in {
    path(Update(wrongId, data)) should be (NotFound(wrongId))
  }

  "NotFound if Delete with wrong ID" in {
    path(Delete(wrongId)) should be (NotFound(wrongId))
  }

  "can Apply bunch of commands" in {
    path(
      Apply( Seq(
        Create(ID(1L), Smt(1, "1", None).asData),
        Create(ID(2L), Smt(2, "2", None).asData),
        Create(ID(3L), Smt(3, "3", None).asData)
      ))
    ) should be (
      Commit( Seq(
        Create(ID(1L), Smt(1, "1", None).asData) -> Created(ID(1L), Smt(1, "1", None).asData),
        Create(ID(2L), Smt(2, "2", None).asData) -> Created(ID(2L), Smt(2, "2", None).asData),
        Create(ID(3L), Smt(3, "3", None).asData) -> Created(ID(3L), Smt(3, "3", None).asData)
      ))
    )

    path(
      Apply( Seq(
        Update(ID(1L), Smt(12, "12", None).asData),
        Delete(ID(2L)),
        Create(ID(4L), Smt(4, "4", None).asData),
        Update(ID(4L), Smt(42, "42", None).asData)
      ))
    ) should be (
      Commit( Map(
          ID(1L) -> Updated(ID(1L), Smt(1, "1", None).asData, Smt(12, "12", None).asData),
          ID(2L) -> Deleted(ID(2L), Smt(2, "2", None).asData),
          ID(4L) -> Created(ID(4L), Smt(42, "42", None).asData)
      ))
    )
  }

  "fail Apply if conflict data" ignore {
    //TODO
  }
}
