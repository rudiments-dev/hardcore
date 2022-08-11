package test.dev.rudiments.hardcore

import dev.rudiments.hardcore.Predicate.All
import dev.rudiments.hardcore._
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TxSpec extends AnyWordSpec with Matchers {
  private val ctx: Memory = new Memory()
  private val tx: Tx = new Tx(ctx)

  private val id = ID("42")

  private val t = Type(Field("a", Bool))
  private val data = Data(t, Seq(true))
  private val data2 = Data(t, Seq(false))

  private val initialCommit = ctx ?? ID("commits") match {
    case Found(All, values) => values
    case _ => fail("Can't read initial commits")
  }

  "can't read non-existing ID in Tx" in { tx ? ID("not exist") should be (NotExist) }

  "can remember Created" in {
    ctx << Commit(Map(id -> Created(data)))
  }
  "can Read if Created" in { ctx ? id should be (Readen(data)) }

  "can read Created in Tx as Readen" in { tx ? id should be (Readen(data)) }

  "can modify in Tx without modification in Context" in {
    (tx *= id -> data2) should be (Updated(data, data2))
    tx ? id should be (Readen(data2))

    ctx ? id should be (Readen(data))
  }

  "can Delete in Tx" in {
    (tx -= id) should be (Deleted(data2))
    tx ? id should be (NotExist)

    ctx ? id should be (Readen(data))
  }

  "can Verify Tx" in {
    tx.>? should be (Valid)
  }

  "can Prepare Change from Tx" in {
    tx.>> should be (Prepared(Commit(Map(id -> Deleted(data)))))
  }

  "can Change Context" in {
    val c = tx.>>.asInstanceOf[Prepared].commit
    ctx << c should be (Committed(c))
  }

  "can see commits of Context" in {
    ctx ?? ID("commits") should be(Found(All, initialCommit ++ Map(
      ID("1240340089") -> Commit(Map(id -> Created(data))),
      ID("-847544541") -> Commit(Map(id -> Deleted(data)))
    )))
  }

  "can prepare commit comparing memory" in {
    val to = Node(Nothing,
      Map(
        ID("example-true") -> Data(Bool, true),
        ID("example-false") -> Data(Bool, false)
      ),
      Map(
        ID("nested-1") -> Node(Nothing,
          Map(ID("inside") -> Data(Text(20), "content of inside"))
        ),
        ID("nested-2") -> Node(Nothing,
          Map.empty[ID, Thing],
          Map(ID("nested-3") -> Node(Nothing,
            Map(ID("deep-inside") -> Data(Number(0, 100), 42))
          ))
        )
      ),
    )

    val compared = Node.empty.compareWith(to)
    val recreated = Node.fromMap(compared)
    recreated should be (to)
  }
}
