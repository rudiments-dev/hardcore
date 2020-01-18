package dev.rudiments.hardcore.data

import dev.rudiments.hardcore.types.{DTO, HardType, ID}

object Batch {
  case class CreateAll  [T <: DTO](batch: Map[ID[T], T]) extends DataCommand[T]
  case class ReplaceAll [T <: DTO](batch: Map[ID[T], T]) extends DataCommand[T]
  case class DeleteAll  [T <: DTO]()                     extends DataCommand[T]

  case class AllCreated [T <: DTO](batch: Map[ID[T], T]) extends DataEvent[T]
  case class AllReplaced[T <: DTO](batch: Map[ID[T], T]) extends DataEvent[T]
  case class AllDeleted [T <: DTO]()                     extends DataEvent[T]

  case class BatchFailed[T <: DTO]() extends DataErrorEvent[T]
}
