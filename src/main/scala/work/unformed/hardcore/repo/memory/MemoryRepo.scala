package work.unformed.hardcore.repo.memory

import cats.effect.IO
import work.unformed.hardcore.dsl._
import work.unformed.hardcore.dsl.ID._
import work.unformed.hardcore.repo.WriteRepository

import scala.collection.parallel.mutable

class MemoryRepo[A](implicit meta: Meta[A]) extends WriteRepository[A] {
  private val content = mutable.ParMap.empty[ID[A], A]

  override def create(draft: A): IO[A] = IO {
    content.get(draft.identify) match {
      case Some(existing) =>
        throw new IllegalArgumentException(s"$existing with key ${draft.identify} already exists")
      case None =>
        content.put(draft.identify, draft)
    }
  }.flatMap(_ => strict(draft.identify))

  override def update(value: A): IO[A] = {
    content.put(value.identify, value)
    strict(value.identify)
  }

  override def delete(id: ID[A]): IO[Unit] = IO { content -= id }

  override def createAll(values: Iterable[A]): IO[Unit] = IO { content ++= values.map(v => (v.identify, v)) }

  override def deleteAll(): IO[Unit] = IO { content.clear() }

  override def get(id: ID[A]): IO[Option[A]] = IO { content.get(id) }

  override def find(query: Query[A]): Result[A] = ???

  override def count(filters: Filter[A]*): Long = content.size.toLong

  override def values(field: String, filters: Filter[A]*): Unit = ???
}
