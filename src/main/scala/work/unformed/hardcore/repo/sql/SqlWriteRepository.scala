package work.unformed.hardcore.repo.sql

import cats.effect.IO
import work.unformed.hardcore.dsl.{Filter, ID, Meta, Query, Result}
import work.unformed.hardcore.repo.{Table, WriteRepository}
import doobie._
import doobie.implicits._
import shapeless.HNil


class SqlWriteRepository[A : Table : Meta](implicit xa: Transactor[IO]) extends WriteRepository[A] {

  private val meta = implicitly[Meta[A]]
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
           |(${values.map(a => "?").mkString(", ")})
         """.stripMargin //todo refact, but auto unwrap is pretty good
      val insertIO = doobie.Update[A](sql).toUpdate0(draft).run
      val getIO = get(meta.identify(draft))

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

  override def create(draft: A): IO[A] = Raw.create(draft)

  override def update(value: A): IO[A] = ???

  override def delete(id: ID[A]): IO[Unit] = for {
    count <- Raw.delete(id).transact(xa)
  } yield ()

  override def deleteAll(): IO[Unit] = ???

  override def get(id: ID[A]): IO[Option[A]] = Raw.get(id)

  override def find(query: Query[A]): Result[A] = ???

  override def count(filters: Filter[A]*): Long = ???

  override def values(field: String, filters: Filter[A]*): Unit = ???
}
