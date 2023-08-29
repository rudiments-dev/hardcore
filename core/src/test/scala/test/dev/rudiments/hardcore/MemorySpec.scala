package test.dev.rudiments.hardcore

import dev.rudiments.hardcore.*
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MemorySpec extends AnyWordSpec with Matchers{
  case class Something(a: Int, b: String)

  "Memory" should {
    import Location._
    given mem: Memory = new Memory
    val sample = Something(42, "forty two")
    val sample2 = Something(43, "forty three")

    "read and create in the location" in {
      mem /? "42" should be (NotFound(ID("42")))
      mem / "42" + sample should be (Created(sample))
      mem /? "42" should be (Readen(sample))
    }

    "update in the location" in {
      mem / "42" * sample2 should be (Updated(sample, sample2))
      mem /? "42" should be (Readen(sample2))
    }

    "delete in the location" in {
      mem - "42" should be (Deleted(sample2))
      mem /? "42" should be (NotFound(ID("42")))
    }
  }
}
