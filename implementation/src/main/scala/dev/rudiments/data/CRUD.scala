package dev.rudiments.data

import dev.rudiments.data.ReadOnly.NotFound
import dev.rudiments.domain.{ID, Instance}
import dev.rudiments.hardcore.{One, Skill}

import scala.collection.parallel

object CRUD {
  case class Create(key: ID, value: Instance) extends One(key) with DataCommand
  case class Created(key: ID, value: Instance) extends One(key) with DataEvent
  case class AlreadyExists(key: ID, value: Instance) extends One(key) with DataError
  case class FailedToCreate(key: ID, value: Instance) extends One(key) with DataError

  def create(implicit content: parallel.mutable.ParMap[ID, Instance]): Skill = {
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


  case class Update(key: ID, value: Instance) extends One(key) with DataCommand
  case class Updated(key: ID, oldvalue: Instance, newvalue: Instance) extends One(key) with DataEvent
  case class FailedToUpdate(key: ID, value: Instance) extends One(key) with DataError

  def update(implicit content: parallel.mutable.ParMap[ID, Instance]): Skill = {
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


  case class Delete(key: ID) extends One(key) with DataCommand
  case class Deleted(key: ID, value: Instance) extends One(key) with DataEvent
  case class FailedToDelete(key: ID, value: Instance) extends One(key) with DataError

  def delete(implicit content: parallel.mutable.ParMap[ID, Instance]): Skill = {
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
