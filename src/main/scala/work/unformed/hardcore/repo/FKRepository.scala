package work.unformed.hardcore.repo

import cats.effect.IO
import work.unformed.hardcore.dsl.{Filter, ID}

trait FKRepository[R, A] extends Repository [A] {}

trait FKReadRepo[R, A] extends FKRepository[R, A] {
  def get(id: ID[R]): IO[Iterable[A]]

  def find(filters: Filter[A]*): IO[(ID[R], Iterable[A])]

  def count(filters: Filter[A]*): IO[Long]

  def values(field: String, filters: Filter[A]*): IO[Iterable[Any]]
}


trait FKWriteRepo[R, A] extends FKReadRepo[R, A] {
  def create(ref: ID[R], values: Iterable[A]): IO[Iterable[A]]

  def update(ref: ID[R], values: Iterable[A]): IO[Iterable[A]] = for {
    _ <- delete(ref)
    res <- create(ref, values)
  } yield res

  def delete(ref: ID[R]): IO[Unit]

  def createAll(values: Map[ID[R], Iterable[A]]): IO[Unit]

  def deleteAll(): IO[Unit]
}
