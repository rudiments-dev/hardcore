package dev.rudiments.hardcore.repo.memory

import dev.rudiments.hardcore.dsl._
import dev.rudiments.hardcore.dsl.ID._
import dev.rudiments.hardcore.repo.PlainRepository
import cats.effect.IO

import scala.collection.parallel.mutable

class MemoryRepo[A](implicit meta: Meta[A]) extends PlainRepository[A] {
  private val content = mutable.ParMap.empty[ID[A], A]

  override def get(key: ID[A]): IO[Result[ID[A], A]] = IO {
    content.get(key)
      .map(value => Result(key, value))
      .getOrElse(throw NotFound(key))
  }

  override def count(filters: Filter[A]*): IO[Long] = IO { content.size }

  override def create(key: ID[A], value: A): IO[Created[ID[A], A]] = IO {
    content.get(key) match {
      case None =>
        content.put(key, value)
        content.get(key) match {
          case Some(created) => Created(key, created)
          case None => throw FailedToCreate(key, value)
        }
      case Some(_) => throw AlreadyExists(value.identify)
    }
  }

  override def update(key: ID[A], value: A): IO[Updated[ID[A], A]] = {
    get(key).map { old =>
      content.put(key, value)
      content.get(key) match {
        case Some(newValue) => Updated(key, old.value, newValue)
        case None => throw FailedToUpdate(key, value)
      }
    }
  }

  override def delete(id: ID[A]): IO[Deleted[ID[A], A]] = {
    get(id).map { old =>
      content -= id
      content.get(id) match {
        case None => Deleted(id, old.value)
        case Some(_) => throw FailedToDelete(id)
      }
    }
  }

  override def createAll(values: Map[ID[A], A])  = IO {
    content ++= values
    AllCreated[ID[A], A](values)
  }

  override def deleteAll() = IO {
    content.clear()
    AllDeleted[ID[A], A]()
  }

  override def find(query: Query[A]): IO[QueryResult[A]] = IO {
    throw NotImplemented("Query API on MemoryRepo")
  }
}
