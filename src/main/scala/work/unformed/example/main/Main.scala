package work.unformed.example.main

import cats.effect.IO
import com.typesafe.scalalogging.LazyLogging
import doobie.Transactor
import doobie.util.{Get, Put, Read}
import work.unformed.example.domain.City
import work.unformed.hardcore.dsl.{ID, Meta, Query}
import work.unformed.hardcore.repo.Table
import work.unformed.hardcore.repo.sql.SqlWriteRepository

import doobie._
import doobie.implicits._
import scala.concurrent.ExecutionContext

object Main extends App with LazyLogging {
  implicit val han = LogHandler.jdkLogHandler

  implicit val meta = Meta[City](city => ID(city.id))

  implicit val table = new Table[City]("City", Seq("id")) {
    override val read: Read[City] = Read[(Long, String, String, String, Long)].map{
      a => City(a._1, a._2, a._3, a._4, a._5)
    }

    override def unwrap: City => Seq[(String, Any)] = city => Seq(
      "id" -> city.id,
      "name" -> city.name,
      "countryCode" -> city.countryCode,
      "district" -> city.district,
      "population" -> city.population
    )

    override implicit val write: Write[City] = Write[(Long, String, String, String, Long)].contramap(
      a => (a.id, a.name, a.countryCode, a.district, a.population)
    )
  }

  implicit val cs = IO.contextShift(ExecutionContext.global)

  // A transactor that gets connections from java.sql.DriverManager and excutes blocking operations
  // on an unbounded pool of daemon threads. See the chapter on connection handling for more info.
  val xa = Transactor.fromDriverManager[IO](
    "com.mysql.cj.jdbc.Driver", // driver classname
    "jdbc:mysql://localhost:3306/hardcore", // connect URL (driver-specific)
    "root",              // user
    "root"                       // password
  )

  val repo = new SqlWriteRepository[City]

  val query = new Query[City]()

//  logger.info(repo.find(query).values.mkString("; "))
  val vrn = City(999, "Voronezh", "RUS", "", 10000)
//  val cr = repo.Raw.create(City(999, "Voronezh", "RUS", "", 10000)).transact(xa).unsafeRunSync()

  logger.info(repo.Raw.create(vrn).transact(xa).unsafeRunSync().toString)
  logger.info(repo.Raw.delete(meta.identify(vrn)).transact(xa).unsafeRunSync().toString())
  logger.info(repo.Raw.get(meta.identify(vrn)).transact(xa).unsafeRunSync().toString())


  val result = for {
    _ <- repo.Raw.create(vrn)
    city <- repo.Raw.get(meta.identify(vrn))
    _ <- repo.Raw.delete(meta.identify(vrn))
    after <- repo.Raw.get(meta.identify(vrn))
  } yield (city, after)

  val rr = result.transact(xa).unsafeRunSync()
  logger.info(s"after create: ${rr._1}")
  logger.info(s"after delete: ${rr._2}")

}
