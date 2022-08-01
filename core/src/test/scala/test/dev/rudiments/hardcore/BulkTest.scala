package test.dev.rudiments.hardcore

import dev.rudiments.hardcore._
import org.junit.Ignore
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@Ignore //TODO mark as long-running tests for separate execution
@RunWith(classOf[JUnitRunner])
class BulkTest extends AnyWordSpec with Matchers {
  private val sampleSize: Int = 1*1000*1000 // 1M
  private val rFill = Range(1, sampleSize + 1)
  private val rRead = Range(sampleSize + 1, 1)

  private val mem: Memory = new Memory()
  private val tx: Tx = new Tx(mem)


  private val t = Type(Field("i", Number(0, sampleSize)), Field("j", Text(10)), Field("k", Text(0)))

  private var commit: Commit = _

  private val initialSize = mem.total.size

  s"can create Commit with $sampleSize records" in {
    rFill.foreach { i =>
      tx += ID(i) -> Data(t, Seq(i, i.toString, ""))
    }
    commit = tx.>>.asInstanceOf[Prepared].commit
    commit.crud.size should be (sampleSize)
  }

  "can update Memory with big Commit" in {
    mem << commit
    mem.total.size should be (sampleSize + initialSize + 1) // commit + initial

    rRead.foreach { i =>
      mem ? ID(i) should be (Readen(Data(t, Seq(i, i.toString, ""))))
    }
  }

  "can update every item in memory" in {
    rFill.foreach { i =>
      val localTx = new Tx(mem)
      localTx.remember(ID(i), Updated(
        Data(t, Seq(i, i.toString, "")),
        Data(t, Seq(-i, "!" + i.toString, "!")),
      ))
      mem << (localTx.>>.asInstanceOf[Prepared].commit)

      mem ? ID(i) should be (Readen(Data(t, Seq(-i, "!" + i.toString, "!"))))
    }
  }
}
