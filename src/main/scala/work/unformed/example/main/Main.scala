package work.unformed.example.main

import com.typesafe.scalalogging.LazyLogging
import doobie.util.{Get, Put, Read}
import work.unformed.example.domain.City
import work.unformed.hardcore.dsl.Query
import work.unformed.hardcore.repo.Table
import work.unformed.hardcore.repo.sql.SqlWriteRepository

object Main extends App with LazyLogging {

  implicit val table = new Table[City]("City") {
    override val dtoRead: Read[City] = Read[(Long, String, String, String, Long)].map{
      a => City(a._1, a._2, a._3, a._4, a._5)
    }
  }

  val repo = new SqlWriteRepository[City]

  val query = new Query[City]()

  logger.info(repo.find(query).values.mkString("; "))
}
