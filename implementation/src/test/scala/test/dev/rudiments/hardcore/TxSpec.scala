package test.dev.rudiments.hardcore

import dev.rudiments.hardcore.Tx.TxOps
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

  private val t = Type(Field("a", Bool))
  private val data = Data(t, Seq(true))
  private val data2 = Data(t, Seq(false))

  private val initial = Commit.beginningOfTime

  "can't read non-existing ID in Tx" in { ID("not exist").? should be (NotExist) }

  "can remember Created" in {
    mem.execute(Commit(Map(id -> Created(data)), null))
  }
  "can Read if Created" in { mem.ask(id, Read) should be (Readen(data)) }

  "can read Created in Tx as Readen" in { id.? should be (Readen(data)) }

  "can modify in Tx without modification on memory" in {
    (id *= data2) should be (Updated(data, data2))
    id.? should be (Readen(data2))

    mem.read(id) should be (Readen(data))
  }

  "can Delete in Tx" in {
    id.-= should be (Deleted(data2))
    id.? should be (NotExist)

    mem.read(id) should be (Readen(data))
  }

  "can Verify Tx" in {
    tx.>? should be (Valid)
  }

  "can Prepare Change from Tx" in {
    tx.>> should be (Prepared(Commit(Map(id -> Deleted(data)), null)))
  }

  "can Change memory" in {
    val c = tx.>>.asInstanceOf[Prepared].commit
    mem << c should be (Committed(c))
  }

  "can see commits of Memory" in {
    mem.commits.toSeq should be(Seq(
      Commit(Map(id -> Created(data)), null),
      Commit(Map(id -> Deleted(data)), null)
    ))
  }

  "init commit" in {
    initial.crud.size should be (0)
    initial.extra.size should be (0)
    initial.basedOn should be (null)
  }
}