package dev.rudiments.hardcore.dsl

trait Command
case object Health extends Command

trait Event
case object HealthOk extends Event

trait Error extends Throwable with Event {
  override def toString: String = this match {
    case p: Product => scala.runtime.ScalaRunTime._toString(p)
    case _ => super.toString
  }
}

case object InProgress extends Error
case class NoHandler(command: Command) extends Error
case class NotImplemented(string: String) extends Error
case class Internal(cause: Throwable) extends Error


trait DataCommand[K, V] extends Command { val key: K }
case class Create[K, V](key: K, value: V) extends DataCommand[K, V]
case class Read[K, V](key: K) extends DataCommand[K, V]
case class Update[K, V](key: K, value: V) extends DataCommand[K, V]
case class Delete[K, V](key: K) extends DataCommand[K, V]

trait DataEvent[K, V] extends Event { val key: K }
case class Created[K, V](key: K, value: V) extends DataEvent[K, V]
case class Result[K, V](key: K, value: V) extends DataEvent[K, V]
case class Updated[K, V](key: K, oldValue: V, newValue: V) extends DataEvent[K, V]
case class Deleted[K, V](key: K, value: V) extends DataEvent[K, V]

trait DataError[K, V] extends Error with DataEvent[K, V]
case class NotFound[K, V](key: K) extends DataError[K, V]
case class AlreadyExists[K, V](key: K) extends DataError[K, V]
case class FailedToCreate[K, V](key: K, value: V) extends DataError[K, V]
case class FailedToUpdate[K, V](key: K, value: V) extends DataError[K, V]
case class FailedToDelete[K, V](key: K) extends DataError[K, V]


trait BatchCommand[K, V] extends Command
case class CreateAll[K, V](values: Map[K, V]) extends BatchCommand[K, V]
case class DeleteAll[K, V]() extends BatchCommand[K, V]

trait BatchEvent[K, V] extends Event
case class AllCreated[K, V](values: Map[K, V]) extends BatchEvent[K, V]
case class AllDeleted[K, V]() extends BatchEvent[K, V]