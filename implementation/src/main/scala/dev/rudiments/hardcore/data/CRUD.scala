package dev.rudiments.hardcore.data

import dev.rudiments.hardcore.types.{DTO, HardType, ID}

object CRUD {
  case class Create [T <: DTO : HardType](key: ID[T], value: T)   extends DataCommand[T]
  case class Update [T <: DTO : HardType](key: ID[T], value: T)   extends DataCommand[T]
  case class Delete [T <: DTO : HardType](key: ID[T])             extends DataCommand[T]

  case class Created  [T <: DTO : HardType](key: ID[T], value: T)                 extends DataEvent[T]
  case class Updated  [T <: DTO : HardType](key: ID[T], oldValue: T, newValue: T) extends DataEvent[T]
  case class Deleted  [T <: DTO : HardType](key: ID[T], value: T)                 extends DataEvent[T]

  case class AlreadyExists  [T <: DTO : HardType](key: ID[T], value: T) extends DataErrorEvent[T]
  case class FailedToCreate [T <: DTO : HardType](key: ID[T], value: T) extends DataErrorEvent[T]
  case class FailedToUpdate [T <: DTO : HardType](key: ID[T], value: T) extends DataErrorEvent[T]
  case class FailedToDelete [T <: DTO : HardType](key: ID[T], value: T) extends DataErrorEvent[T]
}
