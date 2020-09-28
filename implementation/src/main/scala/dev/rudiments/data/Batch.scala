package dev.rudiments.data

import dev.rudiments.data.CRUD.{Created, Deleted, Updated}
import dev.rudiments.data.ReadOnly.Found
import dev.rudiments.domain.{ID, Instance}

import scala.collection.parallel

object Batch {
  case class CreateAll  (batch: Map[ID, Instance]) extends DataCommand
  @deprecated("Bulk operations should results with Commit")
  case class AllCreated (batch: Map[ID, Instance]) extends DataEvent

  def createAll(implicit content: parallel.mutable.ParMap[ID, Instance]): DataSkill = {
    case CreateAll(batch) =>
      try {
        if((batch -- content.keys).size != batch.size) {
          BatchFailed().toEither
        } else {
          content ++= batch
          Commit(batch.map { case (id, value) => id -> Created(id, value) }).toEither
        }

      } catch {
        case _: Exception => BatchFailed().toEither
      }
  }


  case class ReplaceAll (batch: Map[ID, Instance]) extends DataCommand
  @deprecated("Bulk operations should results with Commit")
  case class AllReplaced(batch: Map[ID, Instance]) extends DataEvent

  def replaceAll(implicit content: parallel.mutable.ParMap[ID, Instance]): DataSkill = {
    case ReplaceAll(batch) =>
      val commit = reconcile(content)(Reconcile(batch))
      content --= content.keysIterator
      content ++= batch
      commit
  }


  case class DeleteAll() extends DataCommand
  @deprecated("Bulk operations should results with Commit")
  case class AllDeleted() extends DataEvent

  def deleteAll(implicit content: parallel.mutable.ParMap[ID, Instance]): DataSkill = {
    case DeleteAll() =>
      try {
        val delete = content.map { case (id, value) => id -> Deleted(id, value) }.toList.toMap
        content --= content.keysIterator
        Commit(delete).toEither
      } catch {
        case _: Exception => BatchFailed().toEither
      }
  }


  case class Reconcile(to: Map[ID, Instance]) extends DataCommand
  case class Commit(state: Map[ID, DataEvent]) extends DataEvent
  case class Restore(from: Commit) extends DataCommand

  def reconcile(implicit content: parallel.mutable.ParMap[ID, Instance]): DataSkill = {
    case Reconcile(to) =>
      val create = (to -- content.keys.toList.toSet).map { case (id, value) => id -> Created(id, value) }
      val delete = (content.toList.toMap -- to.keys).map { case (id, value) => id -> Deleted(id, value) }
      val update = to.filterKeys(content.contains).map   {
        case (id, value) if value == content(id) => id -> Found(id, value)
        case (id, value) if value != content(id) => id -> Updated(id, content(id), value)
      }
      Commit(create ++ update ++ delete).toEither
  }

  case class BatchFailed() extends DataErrorEvent
}
