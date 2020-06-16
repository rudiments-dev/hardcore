package dev.rudiments.data

import dev.rudiments.hardcore.flow.{BulkMutate, BulkMutated}
import dev.rudiments.hardcore.types.{ID, Instance}

import scala.collection.parallel

object Batch {
  case class CreateAll  (batch: Map[ID, Instance]) extends DataCommand with BulkMutate
  case class AllCreated (batch: Map[ID, Instance]) extends DataEvent with BulkMutated

  def createAll(implicit content: parallel.mutable.ParMap[ID, Instance]): DataSkill = {
    case CreateAll(batch) =>
      try {
        content ++= batch //TODO fix replacing if matched ID
        AllCreated(batch)
      } catch {
        case _: Exception => BatchFailed()
      }
  }


  case class CreateAllAuto  (batch: Seq[Instance]) extends DataCommand with BulkMutate
  case class AllAutoCreated (batch: Map[ID, Instance]) extends DataEvent with BulkMutated

  def createAllAuto(generator: () => ID)(implicit content: parallel.mutable.ParMap[ID, Instance]): DataSkill = {
    case CreateAllAuto(batch) =>
      try {
        val withAuto = batch.map(i => (generator(), i)).toMap
        content ++= withAuto
        AllAutoCreated(withAuto)
      } catch {
        case _: Exception => BatchFailed()
      }
  }


  case class ReplaceAll (batch: Map[ID, Instance]) extends DataCommand with BulkMutate
  case class AllReplaced(batch: Map[ID, Instance]) extends DataEvent with BulkMutated

  def replaceAll(implicit content: parallel.mutable.ParMap[ID, Instance]): DataSkill = {
    case ReplaceAll(batch) =>
      try {
        content --= content.keysIterator
        content ++= batch
        AllReplaced(batch)
      } catch {
        case e: Exception => BatchFailed()
      }
  }


  case object DeleteAll extends DataCommand with BulkMutate
  case object AllDeleted extends DataEvent with BulkMutated

  def deleteAll(implicit content: parallel.mutable.ParMap[ID, Instance]): DataSkill = {
    case DeleteAll =>
      try {
        content --= content.keysIterator
        AllDeleted
      } catch {
        case _: Exception => BatchFailed()
      }
  }

  case class BatchFailed() extends DataErrorEvent
}
