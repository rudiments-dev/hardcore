package work.unformed.hardcore.repo

import work.unformed.hardcore.dsl.{Boundary, DAO}

trait DataBase extends Boundary {
  override def add[DTO](dao: DAO[DTO]): Unit = ???

  override def make[DTO](): Unit = ???

  override def get[DTO]: DAO[DTO] = ???
}

class Table[DTO] {}

class MySQL extends DataBase {}
class Oracle extends DataBase {}
class PostgreSQL extends DataBase {}
class H2 extends DataBase {}