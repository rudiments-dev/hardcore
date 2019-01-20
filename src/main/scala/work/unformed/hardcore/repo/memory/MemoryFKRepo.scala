package work.unformed.hardcore.repo.memory

import work.unformed.hardcore.dsl._
import work.unformed.hardcore.repo.FKWriteRepo
import cats.implicits._

import scala.collection.parallel.mutable

class MemoryFKRepo[R, A] extends FKWriteRepo[R, A] {
  private val content = mutable.ParMap.empty[ID[R], Iterable[A]]

  override def get(id: ID[R]): Either[Error[A], FKResult[R, A]] = {
    FKResult(id, content.getOrElse(id, Iterable.empty)).asRight
  }

  override def find(filters: Filter[A]*): (ID[R], Iterable[A]) = ???

  override def count(filters: Filter[A]*): Long = content.size

  override def create(ref: ID[R], values: Iterable[A]): Either[Error[A], FKCreated[R, A]] = {
    content.get(ref) match {
      case Some(_) => FKAlreadyExists[R, A](ref).asLeft
      case None =>
        content.put(ref, values)
        content.get(ref) match {
          case Some(created) => FKCreated(ref, created).asRight
          case None => FKFailedToCreate(ref, values).asLeft
        }
    }
  }

  override def update(ref: ID[R], values: Iterable[A]): Either[Error[A], FKUpdated[R, A]] = {
    get(ref).flatMap { old =>
      content.put(ref, values)
      content.get(ref) match {
        case Some(newValues) => FKUpdated[R, A](ref, old.values, newValues).asRight
        case None => FKFailedToUpdate[R, A](ref, values).asLeft
      }
    }
  }

  override def delete(ref: ID[R]): Either[Error[A], FKDeleted[R, A]] = {
    get(ref).flatMap { old =>
      content -= ref
      content.get(ref) match {
        case None => FKDeleted[R, A](ref, old.values).asRight
        case Some(_) => FKFailedToDelete[R, A](ref).asLeft
      }
    }
  }

  override def createAll(values: Map[ID[R], Iterable[A]]): Either[Error[A], FKBatchCreated[R, A]] = {
    content ++= values
    FKBatchCreated(values).asRight
  }

  override def deleteAll(): Either[Error[A], FKAllDeleted[R, A]] = {
    content.clear()
    FKAllDeleted[R, A]().asRight
  }
}
