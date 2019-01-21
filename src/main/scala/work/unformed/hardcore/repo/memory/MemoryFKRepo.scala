package work.unformed.hardcore.repo.memory

import cats.effect.IO
import work.unformed.hardcore.dsl.{Filter, ID}
import work.unformed.hardcore.repo.FKWriteRepo

import scala.collection.parallel.mutable

class MemoryFKRepo[R, A] extends FKWriteRepo[R, A] {
  private val content = mutable.ParMap.empty[ID[R], Iterable[A]]

  override def get(id: ID[R]): IO[Iterable[A]] = IO { content.getOrElse(id, Iterable.empty) }

  override def find(filters: Filter[A]*): IO[(ID[R], Iterable[A])] = ???

  override def count(filters: Filter[A]*): IO[Long] = IO { content.size }

  override def values(field: String, filters: Filter[A]*): IO[Iterable[Any]] = ???

  override def create(ref: ID[R], values: Iterable[A]): IO[Iterable[A]] = IO {
    content.get(ref) match {
      case Some(existing) =>
        throw new IllegalArgumentException(s"[${existing.mkString(", ")}] with key $ref already exists")
      case None =>
        content.put(ref, values)
        content(ref)
    }
  }

  override def delete(ref: ID[R]): IO[Unit] = IO { content -= ref }

  override def createAll(values: Map[ID[R], Iterable[A]]): IO[Unit] = IO { content ++= values }

  override def deleteAll(): IO[Unit] = IO { content.clear() }
}
