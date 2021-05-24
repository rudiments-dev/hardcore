package dev.rudiments.hardcore


sealed trait CrudPlus[K, V]

case class Count[K, V](predicate: Predicate = All) extends Query with CrudPlus[K, V]
case class Read[K, V](key: ID[K]) extends Query with CrudPlus[K, V]
case class Find[K, V](predicate: Predicate = All) extends Query with CrudPlus[K, V]
case class Reconcile[K, V](to: Map[ID[K], V]) extends Query with CrudPlus[K, V]


sealed abstract class DataCommand[K, V](val id: ID[K]*) extends Command with CrudPlus[K, V]
case class Create[K, V](key: ID[K], value: V) extends DataCommand[K, V](key)
case class Update[K, V](key: ID[K], value: V) extends DataCommand[K, V](key)
case class Upsert[K, V](key: ID[K], value: V) extends DataCommand[K, V](key)
case class Move[K, V](oldKey: ID[K], newKey: ID[K], value: V) extends DataCommand[K, V](oldKey, newKey)
case class Copy[K, V](oldKey: ID[K], newKey: ID[K], value: V) extends DataCommand[K, V](oldKey, newKey)
case class Delete[K, V](key: ID[K]) extends DataCommand[K, V](key)

case class CreateAll[K, V](batch: Map[ID[K], V]) extends DataCommand[K, V](batch.keys.toSeq: _*)
case class ReplaceAll[K, V](batch: Map[ID[K], V]) extends DataCommand[K, V](batch.keys.toSeq: _*)
case class DeleteUsing[K, V](predicate: Predicate = All) extends Command with CrudPlus[K, V]

case class Apply[K, V](what: Commit[K, V]) extends DataCommand(what.state.keys.toSeq: _*)


case class Counted[K, V](total: Long) extends Report with CrudPlus[K, V]
case class Readen[K, V](key: ID[K], value: V) extends Report with CrudPlus[K, V]
case class Found[K, V](content: Map[ID[K], V]) extends Report with CrudPlus[K, V]

sealed abstract class DataEvent[K, V](val id: ID[K]*) extends Event with CrudPlus[K, V]
object DataEvent {
  def unapply[K, V](arg: DataEvent[K, V]): Option[ID[K]] = { //TODO unapply for many args and paths
    if (arg.id.size == 1) arg.id.headOption else None
  }
}

case class Created[K, V](key: ID[K], value: V) extends DataEvent[K, V](key)
case class Updated[K, V](key: ID[K], oldValue: V, newValue: V) extends DataEvent[K, V](key)
case class Moved[K, V](oldKey: ID[K], oldValue: V, newKey: ID[K], newValue: V) extends DataEvent[K, V](oldKey, newKey)
case class Copied[K, V](oldKey: ID[K], oldValue: V, newKey: ID[K], newValue: V) extends DataEvent[K, V](oldKey, newKey)
case class Deleted[K, V](key: ID[K], value: V) extends DataEvent[K, V](key)

case class Commit[K, V](state: Map[ID[K], DataEvent[K, V]]) extends DataEvent[K, V](state.keys.toSeq: _*)


sealed abstract class DataError[K, V](val id: ID[K]*) extends Error with CrudPlus[K, V]

case class NotFound[K, V](key: ID[K]) extends DataError(key)
case class AlreadyExists[K, V](key: ID[K], value: V) extends DataError(key)

case class FailedToCreate[K, V](key: ID[K], value: V) extends DataError(key)
case class FailedToUpdate[K, V](key: ID[K], value: V) extends DataError(key)
case class FailedToDelete[K, V](key: ID[K], value: V) extends DataError(key)

case class BatchFailed[K, V]() extends Error with CrudPlus[K, V]
