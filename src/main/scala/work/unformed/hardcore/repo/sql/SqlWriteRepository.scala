package work.unformed.hardcore.repo.sql

import work.unformed.hardcore.dsl.{Filter, ID, Query, Result}
import work.unformed.hardcore.repo.{Table, WriteRepository}
import doobie._
import doobie.implicits._
import cats._
import cats.data._
import cats.effect.IO
import cats.implicits._
import scala.concurrent.ExecutionContext

class SqlWriteRepository[A : Table] extends WriteRepository[A] {
  implicit val han = LogHandler.jdkLogHandler

  implicit val cs = IO.contextShift(ExecutionContext.global)

  // A transactor that gets connections from java.sql.DriverManager and excutes blocking operations
  // on an unbounded pool of daemon threads. See the chapter on connection handling for more info.
  val xa = Transactor.fromDriverManager[IO](
    "com.mysql.cj.jdbc.Driver", // driver classname
    "jdbc:mysql://localhost:3306/hardcore", // connect URL (driver-specific)
    "root",              // user
    "root"                       // password
  )

  private val table = implicitly[Table[A]]
  import table.{dtoRead}

  override def create(draft: A): A = ???

  override def update(value: A): A = ???

  override def delete(id: ID[A]): Unit = ???

  override def deleteAll(): Unit = ???

  override def get(id: ID[A]): A = ???

  override def option(id: ID[A]): Option[A] = ???

  override def find(query: Query[A]): Result[A] = {
    val name = table.tableName
    val sql = sql"SELECT * FROM City".query[A].to[Seq]
    Result(query, sql.transact(xa).unsafeRunSync())
  }

  override def count(): Long = ???

  override def values(field: String, filters: Filter[A]*): Unit = ???

  class Raw {
    def create(draft: A): ConnectionIO[A] = {

    }
  }
}
