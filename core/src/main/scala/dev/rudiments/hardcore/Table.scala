package dev.rudiments.hardcore

import scala.collection.mutable
import scala.reflect.ClassTag

class Table[R : ClassTag, C, K : ClassTag] (
  val schema: Seq[(C, Predicate)] = Seq.empty,
  val log: mutable.Buffer[(K, CUD[K, R])] = mutable.Buffer.empty,
  val state: mutable.Map[K, R] = mutable.Map.empty
) extends CRUD[K, R] {

  override def read(where: K): Readen[R] | NotFound[K] = state.get(where) match {
    case Some(found) => Readen(found)
    case None => NotFound(where)
  }

  override def size: Int = state.size

  override def apply(where: K, what: Evt): Rep = whatIf(where, what) match {
    case c@Created(v: R) =>
      log += (where -> c.asInstanceOf[Created[R]]) //can do it without .asInstanceOf ?
      state += (where -> v)
      c
    case u@Updated(_, v: R) =>
      log += (where -> u.asInstanceOf[Updated[R]])
      state += (where -> v)
      u
    case d@Deleted(old: R) =>
      log += (where -> d.asInstanceOf[Deleted[R]])
      state -= where
      d
    case m@Moved(v: R, to: K) =>
      log += (where -> m.asInstanceOf[Moved[K, R]])
      state -= where
      state += (to -> v)
      m
    case err: Report => err
    case other => throw new IllegalArgumentException(s"Should never happen: $other")
  }

  private val splitRep: PartialFunction[(K, Rep), Either[(K, Report), (K, Evt)]] = {
    case (k, r: Report) => Left(k -> r)
    case (k, e: Evt) => Right(k -> e)
    case (k, other) => throw new IllegalArgumentException(s"Should never happen: $other")
  }

  override def apply(tx: Tx[K, R]): TxReport[K, R] = {
    val branch = new Table[R, C, K](schema, mutable.Buffer.empty, this.state.clone())
    val (err: Seq[(K, Report)], oks: Seq[(K, Evt)]) = tx.events.partitionMap { (k, evt) =>
      splitRep(k -> branch.whatIf(k, evt))
    }
    if(err.isEmpty) {
      val report = oks.partitionMap { (k, evt) => splitRep(k -> this.apply(k, evt)) }
      TxReport(report._2, report._1)
    } else {
      TxReport(Seq.empty, err)
    }
  }
}

object Table {
  def empty[R : ClassTag, K : ClassTag] = new Table[R, String, K](
    Seq.empty, mutable.Buffer.empty, mutable.Map.empty
  )
}
