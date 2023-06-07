package test.dev.rudiments.hardcore

import dev.rudiments.hardcore._
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TableSpec extends AnyWordSpec with Matchers {
  case class Something(a: String, i: Int)
  private val s1 = Something("abc", 42)
  private val s2 = Something("cde", 24)

  private val t = Table.empty[String, Something]

  "Node" should {
    "created empty" in {
      t.size should be (0)
    }

    "add something" in {
      t + ("42" -> s1) should be (Created(s1))
      t.size should be (1)
    }

    "update something" in {
      t * ("42" -> s2) should be(Updated(s1, s2))
      t.size should be(1)
    }

    "move something" in {
      t >> ("42" -> "24") should be(Moved(s2, "24"))
      t.size should be(1)
    }

    "delete something" in {
      t - "24" should be(Deleted(s2))
      t.size should be(0)
    }

    "apply commit" in {
      val pairs = (1 to 10).map (i => i.toString -> Created(Something(i.toHexString, i)))
      t(Tx(pairs:_*)) should be(TxReport(pairs, Seq.empty))
      t.size should be(10)
    }
//
//    "create nested node" ignore {
//      t >+ "n" -> Table.empty[Something, String] should be (Created(Node.empty))
//      t.size should be(11)
//    }
//
//    "put nested values" in {
//      val p = ID("n") / ID("123")
//      t >+ p -> s1 should be (Created(s1))
//      t.size should be (11)
//      t.read(p) should be (Readen(s1))
//    }
//
//    "apply nested commits" in {
//      val pairs = (24 to 42).map (i => ID(i.toString) -> Created(Something(i.toHexString, i)))
//      t(Commit(ID("n") -> Commit(pairs:_*))) should be (Commit(ID("n") -> Commit(pairs:_*)))
//      t.state(ID("n")).asInstanceOf[Node].size should be (20)
//    }
  }
}
