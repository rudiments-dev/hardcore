package dev.rudiments.hardcore.data

import dev.rudiments.hardcore.data.ReadOnly.NotFound
import dev.rudiments.hardcore.types.{DTO, ID}

import scala.collection.parallel

object CRUD {
  case class Create[T <: DTO](key: ID[T], value: T) extends DataCommand[T]
  case class Created[T <: DTO](key: ID[T], value: T) extends DataEvent[T]
  case class AlreadyExists[T <: DTO](key: ID[T], value: T) extends DataErrorEvent[T]
  case class FailedToCreate[T <: DTO](key: ID[T], value: T) extends DataErrorEvent[T]
  def create[T <: DTO](implicit content: parallel.mutable.ParMap[ID[T], T]): DataSkill[T] = {
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
  }

  case class CreateAuto[T <: DTO](value: T) extends DataCommand[T]
  case class FailedToCreateAuto[T <: DTO](key: ID[T], value: T) extends DataErrorEvent[T]

  def createAuto[T <: DTO](generator: () => ID[T])(implicit content: parallel.mutable.ParMap[ID[T], T]): DataSkill[T] = {
    case CreateAuto(value) =>
      val key = generator()
      content.put(key, value)
      content.get(key) match {
        case Some(created) => Created(key, created)
        case None => FailedToCreateAuto(key, value)
      }
  }

  case class Update[T <: DTO](key: ID[T], value: T) extends DataCommand[T]
  case class Updated[T <: DTO](key: ID[T], oldValue: T, newValue: T) extends DataEvent[T]
  case class FailedToUpdate[T <: DTO](key: ID[T], value: T) extends DataErrorEvent[T]
  def update[T <: DTO](implicit content: parallel.mutable.ParMap[ID[T], T]): DataSkill[T] = {
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
  }

  case class Delete[T <: DTO](key: ID[T]) extends DataCommand[T]
  case class Deleted[T <: DTO](key: ID[T], value: T) extends DataEvent[T]
  case class FailedToDelete[T <: DTO](key: ID[T], value: T) extends DataErrorEvent[T]
  def delete[T <: DTO](implicit content: parallel.mutable.ParMap[ID[T], T]): DataSkill[T] = {
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
  }
}
