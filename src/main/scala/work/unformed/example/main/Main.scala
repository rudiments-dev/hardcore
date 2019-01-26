package work.unformed.example.main

import cats.effect.IO
import com.typesafe.scalalogging.LazyLogging
import doobie.Transactor
import doobie.util.{Get, Put, Read}
import work.unformed.example.domain.City
import work.unformed.hardcore.dsl.{ID, Meta, Query}
import work.unformed.hardcore.repo.Table
import work.unformed.hardcore.repo.sql.SqlWriteRepo

import doobie._
import doobie.implicits._
import scala.concurrent.ExecutionContext

object Main extends App with LazyLogging {

  implicit val meta = Meta[City](city => ID(city.id))

  implicit val write: Write[City] = Write[(Long, String, String, String, Long)].contramap(
    a => City.unapply(a).get
  )
  implicit val read: Read[City] = Read[(Long, String, String, String, Long)].map{
    a => City(a._1, a._2, a._3, a._4, a._5)
  }

  implicit val table = new Table[City]("City", Seq("id")) {

    override def unwrap: City => Seq[(String, Any)] = city => Seq(
      "id" -> city.id,
      "name" -> city.name,
      "countryCode" -> city.countryCode,
      "district" -> city.district,
      "population" -> city.population
    )
  }

  implicit val cs = IO.contextShift(ExecutionContext.global)

  implicit val xa = Transactor.fromDriverManager[IO](
    "com.mysql.cj.jdbc.Driver",
    "jdbc:mysql://localhost:3306/hardcore",
    "root",
    "root"
  )

  val repo = new SqlWriteRepo[City]()

  val query = new Query[City]()

  val vrn = City(999, "Voronezh", "RUS", "", 10000)

  logger.info(table.create(vrn).transact(xa).unsafeRunSync().toString)
  logger.info(table.delete(meta.identify(vrn)).transact(xa).unsafeRunSync().toString)
//  logger.info(table.get(meta.identify(vrn)).transact(xa).unsafeRunSync().toString)


  val result = for ч{
    _ <- table.create(vrn)
    city <- table.get(meta.identify(vrn))
    _ <- table.delete(meta.identify(vrn))
    after <- table.get(meta.identify(vrn))
  } yield (city, after)

  val rr = result.transact(xa).unsafeRunSync()
  logger.info(s"after create: ${rr._1}")
  logger.info(s"after delete: ${rr._2}")

}
