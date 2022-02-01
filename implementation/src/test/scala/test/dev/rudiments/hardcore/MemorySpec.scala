package test.dev.rudiments.hardcore

import dev.rudiments.hardcore._
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner
import test.dev.rudiments.Smt

@RunWith(classOf[JUnitRunner])
class MemorySpec extends AnyWordSpec with Matchers {
  private implicit val space: Space = new Space
  private val agent = new Memory(All, All)
  private val data = Smt(13, "test", None).asData
  private val id = ID(42)

  "NotFound by ID" in {
    agent << Read(id) should be (NotFound(id))
    agent >> id should be (NotFound(id))
  }

  "can Create" in {
    agent << Create(id, data) should be (Created(id, data))
  }

  "can Read existing ID" in {
    agent >> id should be (Readen(id, data))
  }

  private val elseData = Smt(31, "sample", Some("thing")).asData
  "can Update" in {
    agent << Update(id, elseData)
  }

  "can Read updated ID" in {
    agent >> id should be (Readen(id, elseData))
  }

  "can Delete" in {
    agent << Delete(id) should be (Deleted(id, elseData))
  }

  "NotFound after Delete" in {
    agent >> id should be (NotFound(id)) // or Deleted(id, elseData) ?
  }

  private val wrongId = ID(666)
  "NotFound if Update with wrong ID" in {
    agent << Update(wrongId, data) should be (NotFound(wrongId))
  }

  "NotFound if Delete with wrong ID" in {
    agent << Delete(wrongId) should be (NotFound(wrongId))
  }

  "can Apply bunch of commands" in {
    agent << Apply( Seq (
      Create(ID(1L), Smt(1, "1", None).asData),
      Create(ID(2L), Smt(2, "2", None).asData),
      Create(ID(3L), Smt(3, "3", None).asData)
    )) should be (
      Commit( Seq(
        Create(ID(1L), Smt(1, "1", None).asData) -> Created(ID(1L), Smt(1, "1", None).asData),
        Create(ID(2L), Smt(2, "2", None).asData) -> Created(ID(2L), Smt(2, "2", None).asData),
        Create(ID(3L), Smt(3, "3", None).asData) -> Created(ID(3L), Smt(3, "3", None).asData)
      ))
    )

    agent << Apply( Seq(
      Update(ID(1L), Smt(12, "12", None).asData),
      Delete(ID(2L)),
      Create(ID(4L), Smt(4, "4", None).asData),
      Update(ID(4L), Smt(42, "42", None).asData)
    )) should be (
      Commit( Map(
        ID(1L) -> Updated(ID(1L), Smt(1, "1", None).asData, Smt(12, "12", None).asData),
        ID(2L) -> Deleted(ID(2L), Smt(2, "2", None).asData),
        ID(4L) -> Created(ID(4L), Smt(42, "42", None).asData)
      ))
    )
  }

  "fail Apply if conflict data" in {

  }
}
