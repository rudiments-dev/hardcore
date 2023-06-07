package dev.rudiments.hardcore

sealed trait Message extends Product
sealed trait In extends Message
sealed trait Out extends Message

trait Command extends In
trait Query extends In
trait Event extends Out
trait Report extends Out
trait Error extends Report

case class Created[V](value: V) extends Event
case class Readen[V](value: V) extends Report
case class Updated[V](old: V, value: V) extends Event
case class Deleted[V](old: V) extends Event
case class Moved[K, V](value: V, to: K) extends Event
case class Tx[K, V](events: (K, CUD[K, V])*) extends Event

case class NotFound[K](where: K) extends Report
case object Identical extends Report
case class TxReport[K, V](ok: Seq[(K, CUD[K, V])], errors: Seq[(K, Report)]) extends Report
case class Conflict(in: Event, actual: Out) extends Error
case class NotSupported(m: Message) extends Error
case class InternalError(t: Throwable) extends Error

type CUD[K, V] = Created[V] | Updated[V] | Deleted[V] | Moved[K, V]
type CUDR[K, V] = CUD[K, V] | Report