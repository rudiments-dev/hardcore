package dev.rudiments.data

import dev.rudiments.hardcore.types.{ID, Instance}

import scala.collection.parallel

object Batch {
  case class CreateAll  (batch: Map[ID, Instance]) extends DataCommand
  case class AllCreated (batch: Map[ID, Instance]) extends DataEvent

  def createAll(implicit content: parallel.mutable.ParMap[ID, Instance]): DataSkill = {
    case CreateAll(batch) =>
      try {
        content ++= batch
        AllCreated(batch)
      } catch {
        case _: Exception => BatchFailed()
      }
  }


  case class CreateAllAuto  (batch: Seq[Instance]) extends DataCommand
  case class AllAutoCreated (batch: Map[ID, Instance]) extends DataEvent

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


  case class ReplaceAll (batch: Map[ID, Instance]) extends DataCommand
  case class AllReplaced(batch: Map[ID, Instance]) extends DataEvent

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


  case class DeleteAll  () extends DataCommand
  case class AllDeleted () extends DataEvent

  def deleteAll(implicit content: parallel.mutable.ParMap[ID, Instance]): DataSkill = {
    case DeleteAll() =>
      try {
        content --= content.keysIterator
        AllDeleted()
      } catch {
        case _: Exception => BatchFailed()
      }
  }

  case class BatchFailed() extends DataErrorEvent
}
