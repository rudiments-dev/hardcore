package dev.rudiments.hardcore.data

import dev.rudiments.hardcore.types.{DTO, HardType, ID}

object Batch {
  case class CreateAll  [T <: DTO : HardType](batch: Map[ID[T], T]) extends DataCommand[T]
  case class ReplaceAll [T <: DTO : HardType](batch: Map[ID[T], T]) extends DataCommand[T]
  case class DeleteAll  [T <: DTO : HardType]()                     extends DataCommand[T]

  case class AllCreated [T <: DTO : HardType](batch: Map[ID[T], T]) extends DataEvent[T]
  case class AllReplaced[T <: DTO : HardType](batch: Map[ID[T], T]) extends DataEvent[T]
  case class AllDeleted [T <: DTO : HardType]()                     extends DataEvent[T]

  case class BatchFailed[T <: DTO : HardType]() extends DataErrorEvent[T]
}
