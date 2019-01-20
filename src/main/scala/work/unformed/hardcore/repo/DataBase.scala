package work.unformed.hardcore.repo

import doobie.util.{Get, Put, Read, Write}
import work.unformed.hardcore.dsl.{Boundary, DAO}

trait DataBase extends Boundary {
  override def add[DTO](dao: DAO[DTO]): Unit = ???

  override def make[DTO](): Unit = ???

  override def get[DTO]: DAO[DTO] = ???
}

abstract class Table[DTO]
(
  val tableName: String,
  val keyColumns: Seq[String]
) {

  implicit val read: Read[DTO]
  implicit val write: Write[DTO]

  def unwrap: DTO => Seq[(String, Any)] //remove with abstraction

  def columns(entity: DTO): Seq[(String, Any)] = unwrap(entity)//CTR
}

class MySQL extends DataBase {}
class Oracle extends DataBase {}
class PostgreSQL extends DataBase {}
class H2 extends DataBase {}