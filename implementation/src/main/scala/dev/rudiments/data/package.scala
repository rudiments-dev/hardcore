package dev.rudiments

import dev.rudiments.domain.{ID, Instance}
import dev.rudiments.hardcore._

package object data {

  sealed trait Data

  case class Count(predicate: Predicate = All) extends Query with Data
  case class Find (key: ID) extends Query with Data
  case class FindAll (predicate: Predicate) extends Query with Data
  case class Reconcile (to: Map[ID, Instance]) extends Query with Data


  sealed abstract class DataCommand(val id: ID*) extends Command with Data
  case class Create (key: ID, value: Instance) extends DataCommand(key)
  case class Update (key: ID, value: Instance) extends DataCommand(key)
  case class Move (oldKey: ID, newKey: ID, value: Instance) extends DataCommand(oldKey, newKey)
  case class Delete (key: ID) extends DataCommand(key)

  case class CreateAll (batch: Map[ID, Instance]) extends DataCommand(batch.keys.toSeq: _*)
  case class ReplaceAll (batch: Map[ID, Instance]) extends DataCommand(batch.keys.toSeq: _*)
  case class DeleteUsing(predicate: Predicate = All) extends Command with Data

  case class Apply (what: Commit) extends DataCommand(what.state.keys.toSeq: _*)


  case class Counted (total: Long) extends Report with Data
  case class Found (key: ID, value: Instance) extends Report with Data
  case class FoundAll (values: Seq[Instance]) extends Report with Data

  sealed abstract class DataEvent(val id: ID*) extends Event with Data
  object DataEvent {
    def unapply(arg: DataEvent): Option[ID] = {
      if (arg.id.size == 1) arg.id.headOption else None
    }
  }

  case class Created (key: ID, value: Instance) extends DataEvent(key)
  case class Updated (key: ID, oldValue: Instance, newValue: Instance) extends DataEvent(key)
  case class Moved (oldKey: ID, oldValue: Instance, newKey: ID, newValue: Instance) extends DataEvent(oldKey, newKey)
  case class Deleted (key: ID, value: Instance) extends DataEvent(key)

  case class Commit (state: Map[ID, DataEvent]) extends DataEvent(state.keys.toSeq: _*)


  sealed abstract class DataError(val id: ID*) extends Error with Data

  case class NotFound (key: ID) extends DataError(key)
  case class AlreadyExists (key: ID, value: Instance) extends DataError(key)

  case class FailedToCreate (key: ID, value: Instance) extends DataError(key)
  case class FailedToUpdate (key: ID, value: Instance) extends DataError(key)
  case class FailedToDelete (key: ID, value: Instance) extends DataError(key)

  case class BatchFailed() extends Error with Data
}