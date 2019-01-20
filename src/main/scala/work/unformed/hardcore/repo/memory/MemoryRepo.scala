package work.unformed.hardcore.repo.memory

import work.unformed.hardcore.dsl._
import work.unformed.hardcore.dsl.ID._
import work.unformed.hardcore.repo.WriteRepository

import cats.implicits._

import scala.collection.parallel.mutable

class MemoryRepo[A](implicit meta: Meta[A]) extends WriteRepository[A] {
  private val content = mutable.ParMap.empty[ID[A], A]

  override def get(id: ID[A]): Either[Error[A], Result[A]] = content.get(id) match {
    case Some(value) => Result(id, value).asRight
    case None => NotFound(id).asLeft
  }

  override def count(filters: Filter[A]*): Long = content.size

  override def create(draft: A): Either[Error[A], Created[A]] = {
    val id = draft.identify
    content.get(id) match {
      case None =>
        content.put(id, draft)
        content.get(id) match {
          case Some(created) => Created(id, created).asRight
          case None => FailedToCreate(id, draft).asLeft
        }
      case Some(_) => AlreadyExists(draft.identify).asLeft
    }
  }

  override def update(value: A): Either[Error[A], Updated[A]] = {
    val id = value.identify
    get(id).flatMap { old =>
      content.put(id, value)
      content.get(id) match {
        case Some(newValue) => Updated(id, old.value, newValue).asRight
        case None => FailedToUpdate(id, value).asLeft
      }
    }
  }

  override def delete(id: ID[A]): Either[Error[A], Deleted[A]] = {
    get(id).flatMap { result =>
      content -= id
      content.get(id) match {
        case None => Deleted(id, result.value).asRight
        case Some(_) => FailedToDelete(id).asLeft
      }
    }
  }

  override def createAll(values: Iterable[A]): Either[Error[A], BatchCreated[A]]  = {
    content ++= values.map(v => (v.identify, v))
    BatchCreated(values).asRight
  }

  override def deleteAll(): Either[Error[A], AllDeleted[A]] = {
    content.clear()
    AllDeleted[A]().asRight
  }

  override def find(query: Query[A]): Either[Error[A], QueryResult[A]] =
    NotImplemented[A]("Query API on MemoryRepo").asLeft
}
