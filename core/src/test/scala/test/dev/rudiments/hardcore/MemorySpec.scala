package test.dev.rudiments.hardcore

import dev.rudiments.hardcore._
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MemorySpec extends AnyWordSpec with Matchers {
  private val ctx: Memory = new Memory()

  private val id: Location = ID("42")
  private val t = Type(Field("a", Bool))
  private val data = Data(t, Seq(true))
  private val data2 = Data(t, Seq(false))

  "NotExist until something Created" in { ctx ? id should be (NotExist) }
  "can Create if NotExist" in { ctx + (id, data) should be (Created(data)) }
  "can remember Created" in { (ctx += id -> data) should be (Created(data)) }
  "can Read if Created" in { ctx ? id should be (Readen(data)) }
  "can Update if Created" in { ctx * (id, data2) should be (Updated(data, data2)) }
  "can Delete if Created" in { ctx - id should be (Deleted(data)) }
  "can remember Updated" in { (ctx *= id -> data2) should be (Updated(data, data2)) }
  "can Read if Updated" in { ctx ? id should be (Readen(data2)) }
  "can Delete if Updated" in { ctx - id should be (Deleted(data2)) }
  "can remember Deleted" in { (ctx -= id) should be (Deleted(data2)) }
  "NotExist if Deleted" in { ctx ? id should be (NotExist) }
}
