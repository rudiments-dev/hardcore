package work.unformed.hardcore.repo

import doobie.util.{Get, Put, Read}
import work.unformed.hardcore.dsl.{Boundary, DAO}

trait DataBase extends Boundary {
  override def add[DTO](dao: DAO[DTO]): Unit = ???

  override def make[DTO](): Unit = ???

  override def get[DTO]: DAO[DTO] = ???
}

abstract class Table[DTO]
(
  val tableName: String
) {

  implicit val dtoRead: Read[DTO]
//  implicit val dtoPut: Put[DTO]

}

class MySQL extends DataBase {}
class Oracle extends DataBase {}
class PostgreSQL extends DataBase {}
class H2 extends DataBase {}