package dev.rudiments.hardcore.data

import dev.rudiments.hardcore.data.ReadOnly.NotFound
import dev.rudiments.hardcore.types.{HardID, ID}

import scala.collection.parallel

object CRUD {
  case class Create[T](key: HardID[T], value: T) extends DataCommand[T]
  case class Created[T](key: HardID[T], value: T) extends DataEvent[T]
  case class AlreadyExists[T](key: HardID[T], value: T) extends DataErrorEvent[T]
  case class FailedToCreate[T](key: HardID[T], value: T) extends DataErrorEvent[T]
  def create[T](implicit content: parallel.mutable.ParMap[HardID[T], T]): DataSkill[T] = {
    case Create(key: HardID[T], value: T) =>
      content.get(key) match {
        case None =>
          content.put(key, value)
          content.get(key) match {
            case Some(created) => Created(key, created).toEither
            case None => FailedToCreate(key, value).toEither
          }
        case Some(v) => AlreadyExists(key, v).toEither
      }
  }

  case class CreateAuto[T](value: T) extends DataCommand[T]
  case class FailedToCreateAuto[T](key: HardID[T], value: T) extends DataErrorEvent[T]

  def createAuto[T](generator: () => HardID[T])(implicit content: parallel.mutable.ParMap[HardID[T], T]): DataSkill[T] = {
    case CreateAuto(value: T) =>
      val key = generator()
      content.put(key, value)
      content.get(key) match {
        case Some(created) => Created(key, created).toEither
        case None => FailedToCreateAuto(key, value).toEither
      }
  }

  case class Update[T](key: HardID[T], value: T) extends DataCommand[T]
  case class Updated[T](key: HardID[T], oldValue: T, newValue: T) extends DataEvent[T]
  case class FailedToUpdate[T](key: HardID[T], value: T) extends DataErrorEvent[T]
  def update[T](implicit content: parallel.mutable.ParMap[HardID[T], T]): DataSkill[T] = {
    case Update(key: HardID[T], value: T) =>
      content.get(key) match {
        case Some(found) =>
          content.put(key, value)
          content.get(key) match {
            case Some(v) if v == value => Updated(key, found, value).toEither
            case Some(v) if v != value => FailedToUpdate(key, v).toEither
            case None => NotFound(key).toEither //TODO think about this error
          }
        case None => NotFound(key).toEither
      }
  }

  case class Delete[T](key: HardID[T]) extends DataCommand[T]
  case class Deleted[T](key: HardID[T], value: T) extends DataEvent[T]
  case class FailedToDelete[T](key: HardID[T], value: T) extends DataErrorEvent[T]
  def delete[T](implicit content: parallel.mutable.ParMap[HardID[T], T]): DataSkill[T] = {
    case Delete(key: HardID[T]) =>
      content.get(key) match {
        case Some(found) =>
          content -= key
          content.get(key) match {
            case None => Deleted(key, found).toEither
            case Some(_) => FailedToDelete(key, found).toEither
          }
        case None => NotFound(key).toEither
      }
  }
}
