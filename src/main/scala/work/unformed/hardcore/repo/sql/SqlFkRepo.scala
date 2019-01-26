package work.unformed.hardcore.repo.sql

import cats.effect.IO
import work.unformed.hardcore.dsl._
import work.unformed.hardcore.repo.{FKWriteRepo, Table}

class SqlFkRepo[R, A](implicit table: Table[(R, A)]) extends FKWriteRepo[R, A] {


  override def get(id: ID[R]): IO[FKResult[R, A]] = ???

//    IO {
//    FKResult(id, )
//  }

  override def create(ref: ID[R], values: Iterable[A]): IO[FKCreated[R, A]] = ???

  override def update(ref: ID[R], values: Iterable[A]): IO[FKUpdated[R, A]] = ???

  override def delete(ref: ID[R]): IO[FKDeleted[R, A]] = ???

  override def createAll(values: Map[ID[R], Iterable[A]]): IO[FKBatchCreated[R, A]] = ???

  override def deleteAll(): IO[FKAllDeleted[R, A]] = ???

  override def find(filters: Filter[A]*): IO[(ID[R], Iterable[A])] = ???

  override def count(filters: Filter[A]*): IO[Long] = ???
}
