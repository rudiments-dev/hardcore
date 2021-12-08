package test.dev.rudiments.hardcore

import dev.rudiments.hardcore._
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner
import test.dev.rudiments.Smt

@RunWith(classOf[JUnitRunner])
class MemorySpec extends AnyWordSpec with Matchers {
  private val agent = new Memory(Type.build[In], Type.build[Out])
  private val data = Smt(13, "test", None).asData
  private val id = ID(42)

  "NotFound by ID" in {
    agent(Read(id)) should be (NotFound(id))
  }

  "can Create" in {
    agent(Create(id, data)) should be (Created(id, data))
  }

  "can Read existing ID" in {
    agent(Read(id)) should be (Readen(id, data))
  }

  private val elseData = Smt(31, "sample", Some("thing")).asData
  "can Update" in {
    agent(Update(id, elseData))
  }

  "can Read updated ID" in {
    agent(Read(id)) should be (Readen(id, elseData))
  }

  "can Delete" in {
    agent(Delete(id)) should be (Deleted(id, elseData))
  }

  "NotFound after Delete" in {
    agent(Read(id)) should be (NotFound(id)) // or Deleted(id, elseData) ?
  }

  private val wrongId = ID(666)
  "NotFound if Update with wrong ID" in {
    agent(Update(wrongId, data)) should be (NotFound(wrongId))
  }

  "NotFound if Delete with wrong ID" in {
    agent(Delete(wrongId)) should be (NotFound(wrongId))
  }
}
