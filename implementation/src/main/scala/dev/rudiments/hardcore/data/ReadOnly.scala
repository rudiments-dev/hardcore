package dev.rudiments.hardcore.data

import dev.rudiments.hardcore.types.{DTO, HardType, ID}

object ReadOnly {
  case class Find   [T <: DTO : HardType](key: ID[T]) extends DataCommand[T]
  case class FindAll[T <: DTO : HardType]()           extends DataCommand[T]
  case class Count  [T <: DTO : HardType]()           extends DataCommand[T]
  //TODO case class Query[T <: DTO : HardType](?) extends DataCommand[T]

  case class Found    [T <: DTO : HardType](key: ID[T], value: T) extends DataEvent[T]
  case class FoundAll [T <: DTO : HardType](values: Seq[T])       extends DataEvent[T]
  case class Counted[T <: DTO : HardType](total: Long)            extends DataEvent[T]

  case class NotFound [T <: DTO : HardType](key: ID[T]) extends DataErrorEvent[T]
}
