package dev.rudiments.data

import dev.rudiments.data.ReadOnly.NotFound
import dev.rudiments.hardcore.types.{ID, Instance}

import scala.collection.parallel

object SoftCRUD {
  case class Create(key: ID, value: Instance) extends DataCommand
  case class Created(key: ID, value: Instance) extends DataEvent
  case class AlreadyExists(key: ID, value: Instance) extends DataErrorEvent
  case class FailedToCreate(key: ID, value: Instance) extends DataErrorEvent
  
  def create(implicit content: parallel.mutable.ParMap[ID, Instance]): DataSkill = {
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


  case class CreateAuto(value: Instance) extends DataCommand
  case class FailedToCreateAuto(key: ID, value: Instance) extends DataErrorEvent

  def createAuto(generator: () => ID)(implicit content: parallel.mutable.ParMap[ID, Instance]): DataSkill = {
    case CreateAuto(value) =>
      val key = generator()
      content.put(key, value)
      content.get(key) match {
        case Some(created) => Created(key, created)
        case None => FailedToCreateAuto(key, value)
      }
  }


  case class Update(key: ID, value: Instance) extends DataCommand
  case class Updated(key: ID, oldvalue: Instance, newvalue: Instance) extends DataEvent
  case class FailedToUpdate(key: ID, value: Instance) extends DataErrorEvent

  def update(implicit content: parallel.mutable.ParMap[ID, Instance]): DataSkill = {
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


  case class Delete(key: ID) extends DataCommand
  case class Deleted(key: ID, value: Instance) extends DataEvent
  case class FailedToDelete(key: ID, value: Instance) extends DataErrorEvent

  def delete(implicit content: parallel.mutable.ParMap[ID, Instance]): DataSkill = {
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
