package dev.rudiments.hardcore.data

import dev.rudiments.hardcore.types._
import dev.rudiments.hardcore.{Adapter, Command, Error, Event}

import scala.collection.parallel.mutable

class DataMemoryAdapter[T <: DTO : Type] extends Adapter[DataCommand[T], DataEvent[T]] {
  private val content = mutable.ParMap.empty[ID[T], T]

  override def isDefinedAt(x: DataCommand[T]): Boolean = x match { //TODO generify
    case _: Create[T] => true
    case _: Find[T] => true
    case _: Update[T] => true
    case _: Delete[T] => true
    case _ => false
  }

  override def apply(cmd: DataCommand[T]): DataEvent[T] = cmd match {

    case Create(key, value) =>
      content.get(key) match {
        case None =>
          content.put(key, value)
          content.get(key) match {
            case Some(created) => Created(key, created)
            case None => FailedToCreate(key, value)
          }
        case Some(v) => AlreadyExists(key, v)
      }

    case Find(key) =>
      content.get(key) match {
        case Some(value) => Found(key, value)
        case None => NotFound(key)
      }

    case Update(key, value) =>
      content.get(key) match {
        case Some(found) =>
          content.put(key, value)
          content.get(key) match {
            case Some(v) if v == value => Updated(key, found, value)
            case Some(v) if v != value => FailedToUpdate(key, v)
            case None => NotFound(key) //TODO think about this error
          }
        case None => NotFound(key)
      }

    case Delete(key) =>
      content.get(key) match {
        case Some(found) =>
          content -= key
          content.get(key) match {
            case None => Deleted(key, found)
            case Some(_) => FailedToDelete(key, found)
          }
        case None => NotFound(key)
      }

    case CreateAll(batch) =>
      try {
        content ++= batch
        AllCreated(batch)
      } catch {
        case _: Exception => BatchFailed()
      }

    case DeleteAll() =>
      try {
        content.clear()
        AllDeleted()
      } catch {
        case _: Exception => BatchFailed()
      }

    case Count() => Counted(content.size)
  }
}


trait DataCommand [T <: DTO] extends Command
case class Create [T <: DTO : Type](key: ID[T], value: T)  extends DataCommand[T]
case class Find   [T <: DTO : Type](key: ID[T])            extends DataCommand[T]
case class Update [T <: DTO : Type](key: ID[T], value: T)  extends DataCommand[T]
case class Delete [T <: DTO : Type](key: ID[T])            extends DataCommand[T]

case class Count[T <: DTO : Type]() extends DataCommand[T]

case class CreateAll[T <: DTO : Type](batch: Map[ID[T], T]) extends DataCommand[T]
case class DeleteAll[T <: DTO : Type]()                     extends DataCommand[T]


trait DataEvent[T] extends Event
case class Created[T <: DTO : Type](key: ID[T], value: T) extends DataEvent[T]
case class Found  [T <: DTO : Type](key: ID[T], value: T) extends DataEvent[T]
case class Updated[T <: DTO : Type](key: ID[T], oldValue: T, newValue: T) extends DataEvent[T]
case class Deleted[T <: DTO : Type](key: ID[T], value: T) extends DataEvent[T]

case class Counted[T <: DTO : Type](total: Long) extends DataEvent[T]

case class AllCreated[T <: DTO : Type](batch: Map[ID[T], T])  extends DataEvent[T]
case class AllDeleted[T <: DTO : Type]()                      extends DataEvent[T]


trait DataErrorEvent[T] extends Error
case class NotFound       [T <: DTO : Type](key: ID[T]) extends DataEvent[T]
case class AlreadyExists  [T <: DTO : Type](key: ID[T], value: T) extends DataEvent[T]
case class FailedToCreate [T <: DTO : Type](key: ID[T], value: T) extends DataEvent[T]
case class FailedToUpdate [T <: DTO : Type](key: ID[T], value: T) extends DataEvent[T]
case class FailedToDelete [T <: DTO : Type](key: ID[T], value: T) extends DataEvent[T]

case class BatchFailed [T <: DTO : Type]() extends DataEvent[T]