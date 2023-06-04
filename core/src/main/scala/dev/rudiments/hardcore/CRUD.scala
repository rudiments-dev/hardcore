package dev.rudiments.hardcore

import scala.collection.immutable.ListMap

sealed trait CRUD {}
object CRUD {}

case class Create(value: Product) extends Command with CRUD
case class Read[K](where: K) extends Query with CRUD
case class Update(old: Product, value: Product) extends Command with CRUD
case class Delete(old: Product) extends Command with CRUD
case class Move[K](from: K, to: K) extends Command with CRUD

case class Created[V](value: V) extends Event with CRUD
case class Readen[V](value: V) extends Report with CRUD
case class Updated[V](old: V, value: V) extends Event with CRUD
case class Deleted[V](old: V) extends Event with CRUD
case class Moved[K, V](value: V, to: K) extends Event with CRUD
case class Tx[K, V](events: (K, CUD[K, V])*) extends Event with CRUD

case class NotFound[K](where: K) extends Report with CRUD
case object Identical extends Report with CRUD
case class TxReport[K, V](ok: Seq[(K, CUD[K, V])], errors: Seq[(K, Report)]) extends Report with CRUD
case class Conflict(in: Event, actual: Out) extends Error with CRUD
case class NotSupported(m: Message) extends Error with CRUD
case class InternalError(t: Throwable) extends Error with CRUD

type CUD[K, V] = Created[V] | Updated[V] | Deleted[V] | Moved[K, V]
type CUDR[K, V] = CUD[K, V] | Report