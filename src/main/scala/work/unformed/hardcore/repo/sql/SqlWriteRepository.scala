package work.unformed.hardcore.repo.sql

import cats.effect.IO
import work.unformed.hardcore.dsl.{Meta => _, Query => _, _}
import work.unformed.hardcore.dsl
import work.unformed.hardcore.repo.{Table, WriteRepository}
import work.unformed.hardcore.dsl.ID._
import doobie._
import doobie.implicits._
import shapeless.HNil

import scala.language.implicitConversions


class SqlWriteRepository[A : Table : dsl.Meta](implicit xa: Transactor[IO]) extends WriteRepository[A] {

  private val table = implicitly[Table[A]]
  import table.{read, write}

  private implicit def connection2IO[T](connectionIO: ConnectionIO[T]): IO[T] = connectionIO.transact(xa)

  object Raw {
    def create(draft: A): ConnectionIO[A] = {
      val values = table.unwrap(draft)
      val sql =
        s"""
           |INSERT INTO ${table.tableName} (${values.map(_._1).mkString(", ")})
           |VALUES
           |(${values.map(_ => "?").mkString(", ")})
         """.stripMargin //todo refact, but auto unwrap is pretty good
      val insertIO = doobie.Update[A](sql).toUpdate0(draft).run
      val getIO = get(draft.identify)

      for {
        _ <- insertIO
        item <- getIO
      } yield item match {
        case Some(a) => a
        case None => throw new RuntimeException()
      }
    }

    def get(id: ID[A]): ConnectionIO[Option[A]] = {
      val keys = table.keyColumns.zip(id.values())
      val sql =
        s"""
          | SELECT *
          | FROM ${table.tableName}
          | WHERE
          | ${keys.map {case (field, value) => s"$field = $value"}.mkString(" AND ")}
        """.stripMargin
      doobie.Query[HNil, A](sql).toQuery0(HNil).option
    }

    def delete(id: ID[A]): ConnectionIO[Int] = {
      val keys = table.keyColumns.zip(id.values())
      val sql = s"""
        |DELETE FROM ${table.tableName}
        |WHERE
        |${keys.map {case (field, value) => s"$field = $value"}.mkString(" AND ")}
      """.stripMargin
      doobie.Update[HNil](sql).toUpdate0(HNil).run
    }

  }

  override def create(draft: A): IO[Created[A]] =
    Raw.create(draft).map(a => Created(a.identify, a))

  override def update(value: A): IO[Updated[A]] = ???

  override def delete(id: ID[A]): IO[Deleted[A]] = Raw.get(id).map {
    case Some(v) =>
      Raw.delete(id)
      Deleted(id, v)
    case None => throw NotFound(id)
  }

  override def createAll(values: Iterable[A]): IO[BatchCreated[A]] = ???

  override def deleteAll(): IO[AllDeleted[A]] = ???

  override def get(id: ID[A]): IO[Result[A]] = Raw.get(id).map {
    case Some(v) => Result(id, v)
    case None => throw NotFound(id)
  }

  override def find(query: dsl.Query[A]): IO[QueryResult[A]] = ???

  override def count(filters: Filter[A]*): IO[Long] = ???
}
