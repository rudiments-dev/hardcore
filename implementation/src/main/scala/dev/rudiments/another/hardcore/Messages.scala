package dev.rudiments.another.hardcore

import dev.rudiments.another.{Command, Event, Query, Report, Error}

sealed trait Data

case class Count(predicate: Predicate = All) extends Query with Data
case class Find[T](key: ID[T]) extends Query with Data
case class FindAll[T](predicate: Predicate) extends Query with Data
case class Reconcile[T](to: Map[ID[T], T]) extends Query with Data


sealed abstract class DataCommand[T](val id: ID[T]*) extends Command with Data
case class Create[T](key: ID[T], value: T) extends DataCommand(key)
case class Update[T](key: ID[T], value: T) extends DataCommand(key)
case class Move[T](oldKey: ID[T], newKey: ID[T], value: T) extends DataCommand(oldKey, newKey)
case class Delete[T](key: ID[T]) extends DataCommand(key)

case class CreateAll[T](batch: Map[ID[T], T]) extends DataCommand(batch.keys.toSeq: _*)
case class ReplaceAll[T](batch: Map[ID[T], T]) extends DataCommand(batch.keys.toSeq: _*)
case class DeleteUsing(predicate: Predicate = All) extends Command with Data

case class Apply[T](what: Commit[T]) extends DataCommand(what.state.keys.toSeq: _*)


case class Counted (total: Long) extends Report with Data
case class Found[T](key: ID[T], value: T) extends Report with Data
case class FoundAll[T](content: Map[ID[T], T]) extends Report with Data

sealed abstract class DataEvent[T](val id: ID[T]*) extends Event with Data
object DataEvent {
  def unapply[T](arg: DataEvent[T]): Option[ID[T]] = {
    if (arg.id.size == 1) arg.id.headOption else None
  }
}

case class Created[T](key: ID[T], value: T) extends DataEvent(key)
case class Updated[T](key: ID[T], oldValue: T, newValue: T) extends DataEvent(key)
case class Moved[T](oldKey: ID[T], oldValue: T, newKey: ID[T], newValue: T) extends DataEvent(oldKey, newKey)
case class Deleted[T](key: ID[T], value: T) extends DataEvent(key)

case class Commit[T](state: Map[ID[T], DataEvent[T]]) extends DataEvent(state.keys.toSeq: _*)


sealed abstract class DataError[T](val id: ID[T]*) extends Error with Data

case class NotFound[T](key: ID[T]) extends DataError(key)
case class AlreadyExists[T](key: ID[T], value: T) extends DataError(key)

case class FailedToCreate[T](key: ID[T], value: T) extends DataError(key)
case class FailedToUpdate[T](key: ID[T], value: T) extends DataError(key)
case class FailedToDelete[T](key: ID[T], value: T) extends DataError(key)

case class BatchFailed() extends Error with Data
