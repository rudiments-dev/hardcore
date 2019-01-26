package work.unformed.hardcore.repo

import doobie.ConnectionIO
import doobie.util.{Get, Put, Read, Write}
import shapeless.HNil
import work.unformed.hardcore.dsl.{Boundary, DAO, ID, Meta}
import work.unformed.hardcore.dsl.ID._


trait DataBase extends Boundary {
  override def add[DTO](dao: DAO[DTO]): Unit = ???

  override def make[DTO](): Unit = ???

  override def get[DTO]: DAO[DTO] = ???
}

abstract class Table[DTO : Meta : Read : Write]
(
  val tableName: String,
  val keyColumns: Seq[String]
) {


  final def create(draft: DTO): ConnectionIO[DTO] = {
    val values = unwrap(draft)
    val sql =
      s"""
         |INSERT INTO $tableName (${values.map(_._1).mkString(", ")})
         |VALUES
         |(${values.map(_ => "?").mkString(", ")})
         """.stripMargin //todo refact, but auto unwrap is pretty good
    val insertIO = doobie.Update[DTO](sql).toUpdate0(draft).run
    val getIO = get(draft.identify)

    for {
      _ <- insertIO
      item <- getIO
    } yield item match {
      case Some(a) => a
      case None => throw new RuntimeException()
    }
  }

  final def get(id: ID[DTO]): ConnectionIO[Option[DTO]] = {
    val keys = keyColumns.zip(id.values())
    val sql =
      s"""
         | SELECT *
         | FROM $tableName
         | WHERE
         | ${keys.map {case (field, value) => s"$field = $value"}.mkString(" AND ")}
        """.stripMargin
    doobie.Query[HNil, DTO](sql).toQuery0(HNil).option
  }

  final def delete(id: ID[DTO]): ConnectionIO[Int] = {
    val keys = keyColumns.zip(id.values())
    val sql = s"""
                 |DELETE FROM $tableName
                 |WHERE
                 |${keys.map {case (field, value) => s"$field = $value"}.mkString(" AND ")}
      """.stripMargin
    doobie.Update[HNil](sql).toUpdate0(HNil).run
  }

  def unwrap: DTO => Seq[(String, Any)] //remove with abstraction

  def columns(entity: DTO): Seq[(String, Any)] = unwrap(entity)//CTR
}

abstract class FKTable[R, A]
(
  val tableName: String,
  val keyColumns: Seq[String]
)(implicit read: Read[A], write: Write[(R, A)]) {


}

class MySQL extends DataBase {}
class Oracle extends DataBase {}
class PostgreSQL extends DataBase {}
class H2 extends DataBase {}