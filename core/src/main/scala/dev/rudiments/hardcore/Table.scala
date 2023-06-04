package dev.rudiments.hardcore

import scala.collection.mutable
import scala.reflect.ClassTag

class Table[R : ClassTag, C, K : ClassTag](
  val schema: Seq[(C, Predicate)] = Seq.empty,
  val log: mutable.Buffer[(K, CUD[K, R])] = mutable.Buffer.empty,
  val state: mutable.Map[K, R] = mutable.Map.empty,
) {

  def read(where: K): Readen[R] | NotFound[K] = state.get(where) match {
    case Some(found) => Readen(found)
    case None => NotFound(where)
  }

  def size: Int = state.size

  type Evt = CUD[K, R]
  type Rep = CUDR[K, R]

  def whatIf(where: K, evt: Evt): Rep = (read(where), evt) match {
    case (Readen(old), u@Updated(v1, v2)) if v1 == old => u
    case (Readen(old), d@Deleted(v)) if v == old => d
    case (Readen(old), m@Moved(v, to: K)) if v == old => read(to) match {
      case _: NotFound[K] => m
      case r: Readen[R] => Conflict(m, r)
    }
    case (_: NotFound[K], c: Created[R]) => c
    case (readen, err) => Conflict(err, readen)
  }

  def apply(where: K, what: Evt): Rep = whatIf(where, what) match {
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

  def apply(tx: Tx[K, R]): TxReport[K, R] = {
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

  def create(where: K, what: R): Rep = this.apply(where, Created(what))
  def + (w: (K, R)): Rep = this.create(w._1, w._2)

  def update(where: K, to: R): Rep = read(where) match {
    case Readen(old) => this.apply(where, Updated(old, to))
    case nf: NotFound[K] => nf
  }
  def * (w: (K, R)): Rep = this.update(w._1, w._2)

  def delete(where: K): Rep = read(where) match {
    case Readen(old) => this.apply(where, Deleted(old))
    case nf: NotFound[K] => nf
  }
  def - (where: K): Rep = this.delete(where)

  def move(from: K, to: K): Rep = (read(from), read(to)) match {
    case (Readen(old), nf: NotFound[K]) => this.apply(from, Moved(old, to))
    case (Readen(old), r@Readen(err)) => Conflict(Moved(old, to), r)
    case (nf: NotFound[K], _) => nf
  }
  def >>(w: (K, K)): Rep = this.move(w._1, w._2)
}

object Table {
  def empty[R : ClassTag, K : ClassTag] = new Table[R, String, K](
    Seq.empty, mutable.Buffer.empty, mutable.Map.empty
  )
}
