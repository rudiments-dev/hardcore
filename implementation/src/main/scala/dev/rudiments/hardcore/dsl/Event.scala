package dev.rudiments.hardcore.dsl

trait Event

trait DataEvent[K, V]

case class Created[K, V](key: K, value: V) extends DataEvent[K, V]
case class Result[K, V](key: K, value: V) extends DataEvent[K, V]
case class Updated[K, V](key: K, oldValue: V, newValue: V) extends DataEvent[K, V]
case class Deleted[K, V](key: K, value: V) extends DataEvent[K, V]

case class AllCreated[K, V](values: Map[K, V]) extends DataEvent[K, V]
case class AllDeleted[K, V]() extends DataEvent[K, V]



trait Error extends Throwable with Event
trait DataError[K, V] extends Error with DataEvent[K, V]

case class NoHandler(command: Command) extends Error
case class NotImplemented(string: String) extends Error
case class Internal(cause: Error) extends Error

case class Failed[K, V](command: DataCommand[K, V], cause: Throwable) extends DataError[K, V]

case class NotFound[K, V](key: K) extends DataError[K, V]
case class AlreadyExists[K, V](key: K) extends DataError[K, V]
case class FailedToCreate[K, V](key: K, value: V) extends DataError[K, V]
case class FailedToUpdate[K, V](key: K, value: V) extends DataError[K, V]
case class FailedToDelete[K, V](key: K) extends DataError[K, V]



trait Command extends Event
trait DataCommand[K, V] extends Command with DataEvent[K, V]

case class Create[K, V](key: K, value: V) extends DataCommand[K, V]
case class Read[K, V](key: K) extends DataCommand[K, V]
case class Update[K, V](key: K, value: V) extends DataCommand[K, V]
case class Delete[K, V](key: K) extends DataCommand[K, V]

case class CreateAll[K, V](values: Map[K, V]) extends DataCommand[K, V]
case class DeleteAll[K, V]() extends DataCommand[K, V]


trait CommandHandler {
  def handle(command: Command): Event
}

trait DataCommandHandler[K, V] {
  def handle(command: DataCommand[K, V]): DataEvent[K, V]
}