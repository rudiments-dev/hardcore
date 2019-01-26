package work.unformed.hardcore.repo.sql

import cats.effect.IO
import work.unformed.hardcore.dsl.{Meta => _, Query => _, _}
import work.unformed.hardcore.dsl
import work.unformed.hardcore.repo.{Table, WriteRepository}
import work.unformed.hardcore.dsl.ID._
import doobie._
import doobie.implicits._
import scala.language.implicitConversions


class SqlWriteRepo[A : Table : dsl.Meta](implicit xa: Transactor[IO]) extends WriteRepository[A] {

  private val table = implicitly[Table[A]]

  private implicit def connection2IO[T](connectionIO: ConnectionIO[T]): IO[T] = connectionIO.transact(xa)

  override def create(draft: A): IO[Created[A]] =
    table.create(draft).map(a => Created(a.identify, a))

  override def update(value: A): IO[Updated[A]] = ???

  override def delete(id: ID[A]): IO[Deleted[A]] = table.get(id).map {
    case Some(v) =>
      table.delete(id)
      Deleted(id, v)
    case None => throw NotFound(id)
  }

  override def createAll(values: Iterable[A]): IO[BatchCreated[A]] = ???

  override def deleteAll(): IO[AllDeleted[A]] = ???

  override def get(id: ID[A]): IO[Result[A]] = table.get(id).map {
    case Some(v) => Result(id, v)
    case None => throw NotFound(id)
  }

  override def find(query: dsl.Query[A]): IO[QueryResult[A]] = ???

  override def count(filters: Filter[A]*): IO[Long] = ???
}
