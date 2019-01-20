package work.unformed.hardcore.repo.memory

import work.unformed.hardcore.dsl._
import work.unformed.hardcore.dsl.ID._
import work.unformed.hardcore.repo.WriteRepository

import cats.effect.IO

import scala.collection.parallel.mutable

class MemoryRepo[A](implicit meta: Meta[A]) extends WriteRepository[A] {
  private val content = mutable.ParMap.empty[ID[A], A]

  override def get(id: ID[A]): IO[Result[A]] = IO {
    content.get(id)
      .map(value => Result(id, value))
      .getOrElse(throw NotFound(id))
  }

  override def count(filters: Filter[A]*): IO[Long] = IO { content.size }

  override def create(draft: A): IO[Created[A]] = IO {
    val id = draft.identify

    content.get(id) match {
      case None =>
        content.put(id, draft)
        content.get(id) match {
          case Some(created) => Created(id, created)
          case None => throw FailedToCreate(id, draft)
        }
      case Some(_) => throw AlreadyExists(draft.identify)
    }
  }

  override def update(value: A): IO[Updated[A]] = {
    val id = value.identify
    get(id).map { old =>
      content.put(id, value)
      content.get(id) match {
        case Some(newValue) => Updated(id, old.value, newValue)
        case None => throw FailedToUpdate(id, value)
      }
    }
  }

  override def delete(id: ID[A]): IO[Deleted[A]] = {
    get(id).map { old =>
      content -= id
      content.get(id) match {
        case None => Deleted(id, old.value)
        case Some(_) => throw FailedToDelete(id)
      }
    }
  }

  override def createAll(values: Iterable[A]): IO[BatchCreated[A]]  = IO {
    content ++= values.map(v => (v.identify, v))
    BatchCreated[A](values)
  }

  override def deleteAll(): IO[AllDeleted[A]] = IO {
    content.clear()
    AllDeleted[A]()
  }

  override def find(query: Query[A]): IO[QueryResult[A]] = IO {
    throw NotImplemented[A]("Query API on MemoryRepo")
  }
}
