package dev.rudiments.hardcore.data

import dev.rudiments.hardcore.data.ReadOnly.NotFound
import dev.rudiments.hardcore.types.ID

import scala.collection.parallel

object CRUD {
  case class Create[T](key: ID[T], value: T) extends DataCommand[T]
  case class Created[T](key: ID[T], value: T) extends DataEvent[T]
  case class AlreadyExists[T](key: ID[T], value: T) extends DataErrorEvent[T]
  case class FailedToCreate[T](key: ID[T], value: T) extends DataErrorEvent[T]
  def create[T](implicit content: parallel.mutable.ParMap[ID[T], T]): DataSkill[T] = {
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

  case class CreateAuto[T](value: T) extends DataCommand[T]
  case class FailedToCreateAuto[T](key: ID[T], value: T) extends DataErrorEvent[T]

  def createAuto[T](generator: () => ID[T])(implicit content: parallel.mutable.ParMap[ID[T], T]): DataSkill[T] = {
    case CreateAuto(value) =>
      val key = generator()
      content.put(key, value)
      content.get(key) match {
        case Some(created) => Created(key, created)
        case None => FailedToCreateAuto(key, value)
      }
  }

  case class Update[T](key: ID[T], value: T) extends DataCommand[T]
  case class Updated[T](key: ID[T], oldValue: T, newValue: T) extends DataEvent[T]
  case class FailedToUpdate[T](key: ID[T], value: T) extends DataErrorEvent[T]
  def update[T](implicit content: parallel.mutable.ParMap[ID[T], T]): DataSkill[T] = {
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

  case class Delete[T](key: ID[T]) extends DataCommand[T]
  case class Deleted[T](key: ID[T], value: T) extends DataEvent[T]
  case class FailedToDelete[T](key: ID[T], value: T) extends DataErrorEvent[T]
  def delete[T](implicit content: parallel.mutable.ParMap[ID[T], T]): DataSkill[T] = {
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
