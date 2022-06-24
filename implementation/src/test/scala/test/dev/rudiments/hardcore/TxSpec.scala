package test.dev.rudiments.hardcore

import dev.rudiments.hardcore.Memory.MemoryOps
import dev.rudiments.hardcore._
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TxSpec extends AnyWordSpec with Matchers {
  private implicit val mem: Memory = new Memory()
  private implicit val tx: Tx = new Tx(mem)

  private val id = ID("42")
  private val id2 = ID("24")

  private val t = Type(Field("a", Bool))
  private val data = Data(t, Seq(true))
  private val data2 = Data(t, Seq(false))

  "NotExist until something Created" in { id.?> should be (NotExist) }
  "can remember Created" in { id.<+(data) should be (Created(data)) }
  "can Read if Created" in { id.?> should be (Readen(data)) }

  "can remember Created in Tx as Readen" in { id }
}
