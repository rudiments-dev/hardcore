package dev.rudiments.hardcore

import dev.rudiments.hardcore.types.{DTO, ID, HardType}

package object data {
  trait DataCommand [T <: DTO] extends Command
  trait DataEvent[T <: DTO] extends Event
  trait DataErrorEvent[T <: DTO] extends DataEvent[T] with Error

  type DataSkill[T <: DTO] = Skill[DataCommand[T], DataEvent[T]]


  case class Create [T <: DTO : HardType](key: ID[T], value: T)   extends DataCommand[T]
  case class Find   [T <: DTO : HardType](key: ID[T])             extends DataCommand[T]
  case class FindAll[T <: DTO : HardType]()                       extends DataCommand[T]
  case class Update [T <: DTO : HardType](key: ID[T], value: T)   extends DataCommand[T]
  case class Delete [T <: DTO : HardType](key: ID[T])             extends DataCommand[T]

  case class CreateAll[T <: DTO : HardType](batch: Map[ID[T], T]) extends DataCommand[T]
  case class DeleteAll[T <: DTO : HardType]()                     extends DataCommand[T]

  case class Count[T <: DTO : HardType]()                         extends DataCommand[T]


  case class Created  [T <: DTO : HardType](key: ID[T], value: T)                 extends DataEvent[T]
  case class Found    [T <: DTO : HardType](key: ID[T], value: T)                 extends DataEvent[T]
  case class FoundAll [T <: DTO : HardType](values: Seq[T])                       extends DataEvent[T]
  case class Updated  [T <: DTO : HardType](key: ID[T], oldValue: T, newValue: T) extends DataEvent[T]
  case class Deleted  [T <: DTO : HardType](key: ID[T], value: T)                 extends DataEvent[T]

  case class AllCreated[T <: DTO : HardType](batch: Map[ID[T], T])              extends DataEvent[T]
  case class AllDeleted[T <: DTO : HardType]()                                  extends DataEvent[T]
  case class Counted[T <: DTO : HardType](total: Long)                          extends DataEvent[T]


  case class NotFound       [T <: DTO : HardType](key: ID[T])           extends DataErrorEvent[T]
  case class AlreadyExists  [T <: DTO : HardType](key: ID[T], value: T) extends DataErrorEvent[T]
  case class FailedToCreate [T <: DTO : HardType](key: ID[T], value: T) extends DataErrorEvent[T]
  case class FailedToUpdate [T <: DTO : HardType](key: ID[T], value: T) extends DataErrorEvent[T]
  case class FailedToDelete [T <: DTO : HardType](key: ID[T], value: T) extends DataErrorEvent[T]

  case class BatchFailed [T <: DTO : HardType]()                        extends DataErrorEvent[T]
}
