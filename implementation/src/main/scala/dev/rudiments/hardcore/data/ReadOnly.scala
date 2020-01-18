package dev.rudiments.hardcore.data

import dev.rudiments.hardcore.types.{DTO, HardType, ID}

object ReadOnly {
  case class Find   [T <: DTO](key: ID[T]) extends DataCommand[T]
  case class FindAll[T <: DTO]()           extends DataCommand[T]
  case class Count  [T <: DTO]()           extends DataCommand[T]
  //TODO case class Query[T <: DTO : HardType](?) extends DataCommand[T]

  case class Found    [T <: DTO](key: ID[T], value: T) extends DataEvent[T]
  case class FoundAll [T <: DTO](values: Seq[T])       extends DataEvent[T]
  case class Counted[T <: DTO](total: Long)            extends DataEvent[T]

  case class NotFound [T <: DTO](key: ID[T]) extends DataErrorEvent[T]
}
