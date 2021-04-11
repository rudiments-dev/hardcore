package dev.rudiments.another.hardcore

import dev.rudiments.another.{Command, Event, Query, Report, Error}

sealed trait Data

case class Count(predicate: Predicate = All) extends Query with Data
case class Find[T](key: Identifier) extends Query with Data
case class FindAll[T](predicate: Predicate = All) extends Query with Data
case class Reconcile[T](to: Map[Identifier, T]) extends Query with Data


sealed abstract class DataCommand[+T](val id: Identifier*) extends Command with Data
case class Create[T](key: Identifier, value: T) extends DataCommand(key)
case class Update[T](key: Identifier, value: T) extends DataCommand(key)
case class Move[T](oldKey: Identifier, newKey: Identifier, value: T) extends DataCommand(oldKey, newKey)
case class Delete[T](key: Identifier) extends DataCommand(key)

case class CreateAll[T](batch: Map[Identifier, T]) extends DataCommand(batch.keys.toSeq: _*)
case class ReplaceAll[T](batch: Map[Identifier, T]) extends DataCommand(batch.keys.toSeq: _*)
case class DeleteUsing(predicate: Predicate = All) extends Command with Data

case class Apply[T](what: Commit[T]) extends DataCommand(what.state.keys.toSeq: _*)


case class Counted (total: Long) extends Report with Data
case class Found[T](key: Identifier, value: T) extends Report with Data
case class FoundAll[T](content: Map[Identifier, T]) extends Report with Data

sealed abstract class DataEvent[+T](val id: Identifier*) extends Event with Data
object DataEvent {
  def unapply[T](arg: DataEvent[T]): Option[Identifier] = {
    if (arg.id.size == 1) arg.id.headOption else None
  }
}

case class Created[T](key: Identifier, value: T) extends DataEvent(key)
case class Updated[T](key: Identifier, oldValue: T, newValue: T) extends DataEvent(key)
case class Moved[T](oldKey: Identifier, oldValue: T, newKey: Identifier, newValue: T) extends DataEvent(oldKey, newKey)
case class Deleted[T](key: Identifier, value: T) extends DataEvent(key)

case class Commit[T](state: Map[Identifier, DataEvent[T]]) extends DataEvent(state.keys.toSeq: _*)


sealed abstract class DataError[+T](val id: Identifier*) extends Error with Data

case class NotFound[T](key: Identifier) extends DataError(key)
case class AlreadyExists[T](key: Identifier, value: T) extends DataError(key)

case class FailedToCreate[T](key: Identifier, value: T) extends DataError(key)
case class FailedToUpdate[T](key: Identifier, value: T) extends DataError(key)
case class FailedToDelete[T](key: Identifier, value: T) extends DataError(key)

case class BatchFailed() extends Error with Data
