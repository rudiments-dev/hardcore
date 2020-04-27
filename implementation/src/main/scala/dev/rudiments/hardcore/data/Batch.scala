package dev.rudiments.hardcore.data

import dev.rudiments.hardcore.types.HardID

import scala.collection.parallel

object Batch {
  case class CreateAll  [T](batch: Map[HardID[T], T]) extends DataCommand[T]
  case class AllCreated [T](batch: Map[HardID[T], T]) extends DataEvent[T]
  def createAll[T](implicit content: parallel.mutable.ParMap[HardID[T], T]): DataSkill[T] = {
    case CreateAll(batch) =>
      try {
        content ++= batch
        AllCreated(batch)
      } catch {
        case _: Exception => BatchFailed()
      }
  }

  case class CreateAllAuto  [T](batch: Seq[T]) extends DataCommand[T]
  case class AllAutoCreated [T](batch: Map[HardID[T], T]) extends DataEvent[T]
  def createAllAuto[T](generator: () => HardID[T])(implicit content: parallel.mutable.ParMap[HardID[T], T]): DataSkill[T] = {
    case CreateAllAuto(batch) =>
      try {
        val withAuto = batch.map(i => (generator(), i)).toMap
        content ++= withAuto
        AllAutoCreated(withAuto)
      } catch {
        case _: Exception => BatchFailed()
      }
  }

  case class ReplaceAll [T](batch: Map[HardID[T], T]) extends DataCommand[T]
  case class AllReplaced[T](batch: Map[HardID[T], T]) extends DataEvent[T]
  def replaceAll[T](implicit content: parallel.mutable.ParMap[HardID[T], T]): DataSkill[T] = {
    case ReplaceAll(batch) =>
      try {
        content --= content.keysIterator
        content ++= batch
        AllReplaced(batch)
      } catch {
        case e: Exception => BatchFailed()
      }
  }

  case class DeleteAll  [T]() extends DataCommand[T]
  case class AllDeleted [T]() extends DataEvent[T]
  def deleteAll[T](implicit content: parallel.mutable.ParMap[HardID[T], T]): DataSkill[T] = {
    case DeleteAll() =>
      try {
        content --= content.keysIterator
        AllDeleted()
      } catch {
        case _: Exception => BatchFailed()
      }
  }

  case class BatchFailed[T]() extends DataErrorEvent[T]
}
