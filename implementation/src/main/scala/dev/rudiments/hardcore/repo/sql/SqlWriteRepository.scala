package dev.rudiments.hardcore.repo.sql

import cats.effect.IO
import dev.rudiments.hardcore.dsl.{Meta => _, Query => _, _}
import dev.rudiments.hardcore.dsl
import dev.rudiments.hardcore.repo.{PlainRepository, Table}
import doobie._
import doobie.implicits._
import shapeless.HNil

import scala.language.implicitConversions


class SqlWriteRepository[A : Table : dsl.Meta](implicit xa: Transactor[IO]) extends PlainRepository[A] {

  private val table = implicitly[Table[A]]
  import table.{read, write}

  private implicit def connection2IO[T](connectionIO: ConnectionIO[T]): IO[T] = connectionIO.transact(xa)

  object Raw {
    def create(key: ID[A], value: A): ConnectionIO[A] = {
      val values = table.unwrap(value)
      val sql =
        s"""
           |INSERT INTO ${table.tableName} (${values.map(_._1).mkString(", ")})
           |VALUES
           |(${values.map(_ => "?").mkString(", ")})
         """.stripMargin //todo refact, but auto unwrap is pretty good
      val insertIO = doobie.Update[A](sql).toUpdate0(value).run
      val getIO = get(key)

      for {
        _ <- insertIO
        item <- getIO
      } yield item match {
        case Some(a) => a
        case None => throw new RuntimeException()
      }
    }

    def get(key: ID[A]): ConnectionIO[Option[A]] = {
      val keys = table.keyColumns.zip(key.values())
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

  override def create(key: ID[A], value: A): IO[Created[ID[A], A]] =
    Raw.create(key, value).map(a => Created(key, a))

  override def update(key:ID[A], value: A): IO[Updated[ID[A], A]] = ???

  override def delete(id: ID[A]): IO[Deleted[ID[A], A]] = Raw.get(id).map {
    case Some(v) =>
      Raw.delete(id)
      Deleted(id, v)
    case None => throw NotFound(id)
  }

  override def createAll(values: Map[ID[A], A]): IO[AllCreated[ID[A], A]] = ???

  override def deleteAll(): IO[AllDeleted[ID[A], A]] = ???

  override def get(key: ID[A]): IO[Result[ID[A], A]] = Raw.get(key).map {
    case Some(v) => Result(key, v)
    case None => throw NotFound(key)
  }

  override def find(query: dsl.Query[A]): IO[QueryResult[A]] = ???

  override def count(filters: Filter[A]*): IO[Long] = ???
}
