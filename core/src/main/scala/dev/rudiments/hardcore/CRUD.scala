package dev.rudiments.hardcore

import scala.collection.immutable.ListMap

trait CRUD[K, V] {
  type Evt = CUD[K, V]
  type Rep = CUDR[K, V]

  def read(where: K): Readen[V] | NotFound[K]
  def apply(where: K, what: Evt): Rep
  def apply(tx: Tx[K, V]): TxReport[K, V]
  def size: Int

  def whatIf(where: K, evt: Evt): Rep = (read(where), evt) match {
    case (Readen(old), u@Updated(v1, v2)) if v1 == old => u
    case (Readen(old), d@Deleted(v)) if v == old => d
    case (Readen(old), m@Moved(v, to: K)) if v == old => read(to) match {
      case _: NotFound[K] => m
      case r: Readen[V] => Conflict(m, r)
    }
    case (_: NotFound[K], c: Created[V]) => c
    case (readen, err) => Conflict(err, readen)
  }

  def create(where: K, what: V): Rep = this.apply(where, Created(what))
  def +(w: (K, V)): Rep = this.create(w._1, w._2)

  def update(where: K, to: V): Rep = read(where) match {
    case Readen(old) => this.apply(where, Updated(old, to))
    case nf: NotFound[K] => nf
  }
  def *(w: (K, V)): Rep = this.update(w._1, w._2)

  def delete(where: K): Rep = read(where) match {
    case Readen(old) => this.apply(where, Deleted(old))
    case nf: NotFound[K] => nf
  }
  def -(where: K): Rep = this.delete(where)

  def move(from: K, to: K): Rep = (read(from), read(to)) match {
    case (Readen(old), nf: NotFound[K]) => this.apply(from, Moved(old, to))
    case (Readen(old), r@Readen(err)) => Conflict(Moved(old, to), r)
    case (nf: NotFound[K], _) => nf
  }
  def >>(w: (K, K)): Rep = this.move(w._1, w._2)
}
