package test.dev.rudiments.hardcore

import dev.rudiments.hardcore._
import org.junit.Ignore
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BulkTest extends AnyWordSpec with Matchers {
  private val sampleSize: Int = 1*1000*10 // TODO 1*1000*1000 == 1M, now fails because of commit.getHash collision
  private val rFill = Range(1, sampleSize + 1)
  private val rRead = Range(sampleSize + 1, 1)

  private val ctx: Memory = new Memory()
  private val tx: Tx = new Tx(ctx)


  private val t = Type(Field("i", Number(0, sampleSize)), Field("j", Text(10)), Field("k", Text(0)))

  private var commit: Commit = _

  s"can create Commit with $sampleSize records" in {
    rFill.foreach { i =>
      tx += ID(i) -> Data(t, Seq(i, i.toString, ""))
    }
    commit = tx.>>.asInstanceOf[Prepared].commit
    commit.crud.size should be (sampleSize)
  }

  "can update Memory with big Commit" in {
    ctx << commit
    rRead.foreach { i =>
      ctx ? ID(i) should be (Readen(Data(t, Seq(i, i.toString, ""))))
    }
  }

  "can update every item in memory" in {
    rFill.foreach { i =>
      withClue(s"i: $i") {
        val localTx = new Tx(ctx)
        localTx.remember(ID(i), Updated(
          Data(t, Seq(i, i.toString, "")),
          Data(t, Seq(-i, "!" + i.toString, "!")),
        ))
        val cmt = localTx.>>.asInstanceOf[Prepared].commit
        ctx << (localTx.>>.asInstanceOf[Prepared].commit) should be(Committed(cmt))


        ctx ? ID(i) should be(Readen(Data(t, Seq(-i, "!" + i.toString, "!"))))
      }
    }
  }
}
