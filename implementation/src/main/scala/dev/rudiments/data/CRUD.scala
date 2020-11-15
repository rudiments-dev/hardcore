package dev.rudiments.data

import dev.rudiments.data.ReadOnly.NotFound
import dev.rudiments.hardcore.flow.{Mutated, Mutates, SideEffect}
import dev.rudiments.domain.{ID, Instance}

import scala.collection.concurrent

object CRUD {
  case class Create(key: ID, value: Instance) extends DataCommand with Mutates
  case class Created(key: ID, value: Instance) extends DataEvent with Mutated
  case class AlreadyExists(key: ID, value: Instance) extends DataErrorEvent
  case class FailedToCreate(key: ID, value: Instance) extends DataErrorEvent

  def create(implicit content: concurrent.Map[ID, Instance]): DataSkill = {
    case Create(key, value) =>
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


  case class CreateAuto(value: Instance) extends DataCommand with SideEffect
  case class FailedToCreateAuto(key: ID, value: Instance) extends DataErrorEvent

  def createAuto(generator: () => ID)(implicit content: concurrent.Map[ID, Instance]): DataSkill = {
    case CreateAuto(value) =>
      val key = generator()
      content.put(key, value)
      content.get(key) match {
        case Some(created) => Created(key, created).toEither
        case None => FailedToCreateAuto(key, value).toEither
      }
  }


  case class Update(key: ID, value: Instance) extends DataCommand with Mutates
  case class Updated(key: ID, oldvalue: Instance, newvalue: Instance) extends DataEvent with Mutated
  case class FailedToUpdate(key: ID, value: Instance) extends DataErrorEvent

  def update(implicit content: concurrent.Map[ID, Instance]): DataSkill = {
    case Update(key, value) =>
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


  case class Delete(key: ID) extends DataCommand with Mutates
  case class Deleted(key: ID, value: Instance) extends DataEvent with Mutated
  case class FailedToDelete(key: ID, value: Instance) extends DataErrorEvent

  def delete(implicit content: concurrent.Map[ID, Instance]): DataSkill = {
    case Delete(key) =>
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
