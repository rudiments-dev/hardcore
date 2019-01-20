package work.unformed.hardcore.repo

import work.unformed.hardcore.dsl._

import cats.effect.IO

trait FKRepository[R, A] extends Repository [A] {}

trait FKReadRepo[R, A] extends FKRepository[R, A] {
  def get(id: ID[R]): IO[FKResult[R, A]]
  def find(filters: Filter[A]*): IO[(ID[R], Iterable[A])] // TODO replace with commands
  def count(filters: Filter[A]*): IO[Long]
}


trait FKWriteRepo[R, A] extends FKReadRepo[R, A] {
  def create(ref: ID[R], values: Iterable[A]): IO[FKCreated[R, A]]
  def update(ref: ID[R], values: Iterable[A]): IO[FKUpdated[R, A]]
  def delete(ref: ID[R]): IO[FKDeleted[R, A]]
  def createAll(values: Map[ID[R], Iterable[A]]): IO[FKBatchCreated[R, A]]
  def deleteAll(): IO[FKAllDeleted[R, A]]
}
