package work.unformed.hardcore.repo.memory

import work.unformed.hardcore.dsl._
import work.unformed.hardcore.repo.FKWriteRepo

import cats.effect._

import scala.collection.parallel.mutable

class MemoryFKRepo[R, A] extends FKWriteRepo[R, A] {
  private val content = mutable.ParMap.empty[ID[R], Iterable[A]]

  override def get(id: ID[R]): IO[FKResult[R, A]] = IO {
    FKResult(id, content.getOrElse(id, Iterable.empty))
  }

  override def find(filters: Filter[A]*): IO[(ID[R], Iterable[A])] = ???

  override def count(filters: Filter[A]*): IO[Long] = IO { content.size }

  override def create(ref: ID[R], values: Iterable[A]): IO[FKCreated[R, A]] = IO {
    content.get(ref) match {
      case Some(_) => throw FKAlreadyExists[R, A](ref)
      case None =>
        content.put(ref, values)
        content.get(ref) match {
          case Some(created) => FKCreated(ref, created)
          case None => throw FKFailedToCreate(ref, values)
        }
    }
  }

  override def update(ref: ID[R], values: Iterable[A]): IO[FKUpdated[R, A]] = {
    get(ref).map { old =>
      content.put(ref, values)
      content.get(ref) match {
        case Some(newValues) => FKUpdated(ref, old.values, newValues)
        case None => throw FKFailedToUpdate(ref, values)
      }
    }
  }

  override def delete(ref: ID[R]): IO[FKDeleted[R, A]] = {
    get(ref).map { old =>
      content -= ref
      content.get(ref) match {
        case None => FKDeleted(ref, old.values)
        case Some(_) => throw FKFailedToDelete(ref)
      }
    }
  }

  override def createAll(values: Map[ID[R], Iterable[A]]): IO[FKBatchCreated[R, A]] = IO {
    content ++= values
    FKBatchCreated(values)
  }

  override def deleteAll(): IO[FKAllDeleted[R, A]] = IO {
    content.clear()
    FKAllDeleted[R, A]()
  }
}
