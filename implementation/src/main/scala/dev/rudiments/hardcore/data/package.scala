package dev.rudiments.hardcore

import dev.rudiments.hardcore.types.{DTO, ID, Type}

package object data {
  trait DataCommand [T <: DTO] extends Command
  trait DataEvent[T <: DTO] extends Event
  trait DataErrorEvent[T <: DTO] extends DataEvent[T] with Error

  type DataSkill[T <: DTO] = Skill[DataCommand[T], DataEvent[T]]


  case class Create [T <: DTO : Type](key: ID[T], value: T)   extends DataCommand[T]
  case class Find   [T <: DTO : Type](key: ID[T])             extends DataCommand[T]
  case class Update [T <: DTO : Type](key: ID[T], value: T)   extends DataCommand[T]
  case class Delete [T <: DTO : Type](key: ID[T])             extends DataCommand[T]

  case class CreateAll[T <: DTO : Type](batch: Map[ID[T], T]) extends DataCommand[T]
  case class DeleteAll[T <: DTO : Type]()                     extends DataCommand[T]

  case class Count[T <: DTO : Type]()                         extends DataCommand[T]


  case class Created[T <: DTO : Type](key: ID[T], value: T)                 extends DataEvent[T]
  case class Found  [T <: DTO : Type](key: ID[T], value: T)                 extends DataEvent[T]
  case class Updated[T <: DTO : Type](key: ID[T], oldValue: T, newValue: T) extends DataEvent[T]
  case class Deleted[T <: DTO : Type](key: ID[T], value: T)                 extends DataEvent[T]

  case class AllCreated[T <: DTO : Type](batch: Map[ID[T], T])              extends DataEvent[T]
  case class AllDeleted[T <: DTO : Type]()                                  extends DataEvent[T]
  case class Counted[T <: DTO : Type](total: Long)                          extends DataEvent[T]


  case class NotFound       [T <: DTO : Type](key: ID[T])           extends DataErrorEvent[T]
  case class AlreadyExists  [T <: DTO : Type](key: ID[T], value: T) extends DataErrorEvent[T]
  case class FailedToCreate [T <: DTO : Type](key: ID[T], value: T) extends DataErrorEvent[T]
  case class FailedToUpdate [T <: DTO : Type](key: ID[T], value: T) extends DataErrorEvent[T]
  case class FailedToDelete [T <: DTO : Type](key: ID[T], value: T) extends DataErrorEvent[T]

  case class BatchFailed [T <: DTO : Type]()                        extends DataErrorEvent[T]
}
