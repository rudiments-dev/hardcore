package dev.rudiments.hardcore.data

import dev.rudiments.hardcore.types.{DTO, ID}

import scala.collection.parallel

object Batch {
  case class CreateAll  [T <: DTO](batch: Map[ID[T], T]) extends DataCommand[T]
  case class AllCreated [T <: DTO](batch: Map[ID[T], T]) extends DataEvent[T]
  def createAll[T <: DTO](implicit content: parallel.mutable.ParMap[ID[T], T]): DataSkill[T] = {
    case CreateAll(batch) =>
      try {
        content ++= batch
        AllCreated(batch)
      } catch {
        case _: Exception => BatchFailed()
      }
  }

  case class ReplaceAll [T <: DTO](batch: Map[ID[T], T]) extends DataCommand[T]
  case class AllReplaced[T <: DTO](batch: Map[ID[T], T]) extends DataEvent[T]
  def replaceAll[T <: DTO](implicit content: parallel.mutable.ParMap[ID[T], T]): DataSkill[T] = {
    case ReplaceAll(batch) =>
      try {
        content --= content.keysIterator
        content ++= batch
        AllReplaced(batch)
      } catch {
        case e: Exception => BatchFailed()
      }
  }

  case class DeleteAll  [T <: DTO]() extends DataCommand[T]
  case class AllDeleted [T <: DTO]() extends DataEvent[T]
  def deleteAll[T <: DTO](implicit content: parallel.mutable.ParMap[ID[T], T]): DataSkill[T] = {
    case DeleteAll() =>
      try {
        content --= content.keysIterator
        AllDeleted()
      } catch {
        case _: Exception => BatchFailed()
      }
  }

  case class BatchFailed[T <: DTO]() extends DataErrorEvent[T]
}
