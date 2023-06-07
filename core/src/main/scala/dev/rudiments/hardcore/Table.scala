package dev.rudiments.hardcore

import scala.collection.mutable
import scala.reflect.ClassTag

class Table[K : ClassTag, V : ClassTag] (
  val schema: Predicate = Nothing,
  val log: mutable.Buffer[(K, CUD[K, V])] = mutable.Buffer.empty,
  val state: mutable.Map[K, V] = mutable.Map.empty
) extends CRUD[K, V] {

  override def read(where: K): Readen[V] | NotFound[K] = state.get(where) match {
    case Some(found) => Readen(found)
    case None => NotFound(where)
  }

  override def size: Int = state.size

  override def apply(where: K, what: Evt): Rep = whatIf(where, what) match {
    case c@Created(v: V) =>
      log += (where -> c.asInstanceOf[Created[V]]) //can do it without .asInstanceOf ?
      state += (where -> v)
      c
    case u@Updated(_, v: V) =>
      log += (where -> u.asInstanceOf[Updated[V]])
      state += (where -> v)
      u
    case d@Deleted(old: V) =>
      log += (where -> d.asInstanceOf[Deleted[V]])
      state -= where
      d
    case m@Moved(v: V, to: K) =>
      log += (where -> m.asInstanceOf[Moved[K, V]])
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

  override def apply(tx: Tx[K, V]): TxReport[K, V] = {
    val branch = this.branch
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

  def branch = new Table[K, V](schema, mutable.Buffer.empty, this.state.clone())
}

object Table {
  def empty[K : ClassTag, V : ClassTag] = new Table[K, V](
    Nothing, mutable.Buffer.empty, mutable.Map.empty
  )
}
