package dev.rudiments.hardcore.sql

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.typesafe.config.{Config, ConfigFactory}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.hardcode.sql.{SQLAdapter, SQLDataHttpPort}
import dev.rudiments.hardcode.sql.schema.{Column, ColumnTypes, Schema, Table}
import dev.rudiments.hardcore.http.{SoftDecoder, SoftEncoder}
import dev.rudiments.hardcore.types._
import io.circe.{Decoder, Encoder}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}
import scalikejdbc.{AutoSession, DBSession, _}

import scala.collection.JavaConverters._

@RunWith(classOf[JUnitRunner])
class SQLDataHttpPortTest extends WordSpec with Matchers with ScalatestRouteTest with FailFastCirceSupport {

  private case class Example(
                              id: Long = Defaults.long,
                              name: String
                            ) extends DTO

  private implicit val actorSystem: ActorSystem = ActorSystem()
  private implicit val t: Type = HardType[Example].copy(primaryKeys = Seq("id")) //todo fix primary keys


  private implicit val en: Encoder[SoftInstance] = SoftEncoder(t)
  private implicit val de: Decoder[SoftInstance] = SoftDecoder(t)
  private val sample = t.softFromHard(Example(42, "sample"))
  private val id = SoftID(t.extract(sample, "id"))
  val config: Config = ConfigFactory.parseMap(Map(
    "driver" -> "org.h2.Driver",
    "url" -> "jdbc:h2:mem:hello",
    "user" -> "user",
    "password" -> "pass",
    "schema" -> "hello"
  ).asJava)
  def initConnectionPool(config: Config): String = {
    val driver =    config.getString("driver")
    val url =       config.getString("url")
    val user =      config.getString("user")
    val password =  config.getString("password")
    Class.forName(driver)
    ConnectionPool.singleton(url, user, password)
    config.getString("schema")
  }
  initConnectionPool(config)





  private val repo: SQLAdapter = new SQLAdapter(DB.connect(), schema = Schema("hello", Map(
    t -> Table("example", Seq(
      Column("id", ColumnTypes.INT, nullable = false, default = false, pk = true),
      Column("name", ColumnTypes.VARCHAR(255), nullable = false, default = false, pk = false)
    ))
  ), Set.empty))

  private val router: SQLDataHttpPort = new SQLDataHttpPort(
    "example",
    "id",
    i => SoftID(t.extract(i, "id")),
    repo
  )

  private val routes = Route.seal(router.routes)

  "should create table schema by name" in {
    implicit val session: DBSession = AutoSession
    sql"CREATE SCHEMA hello".execute().apply()
    sql"SET SCHEMA hello".execute().apply()

    sql"""CREATE TABLE example (
         |      id IDENTITY PRIMARY KEY,
         |      name VARCHAR(255) NOT NULL,
         |)""".stripMargin.execute().apply()

    sql"""SELECT * FROM example""".execute().apply()
  }

  "no element by ID" in {
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.NotFound)
    }
  }

  "put item into repository" in {
    Post("/example", sample) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
      responseAs[SoftInstance] should be (sample)
    }
  }

  "update item in repository" in {
    Put("/example/42", SoftInstance(42L, "test")) ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[SoftInstance] should be (SoftInstance(42, "test"))
    }
  }

  "second create makes conflict" in {
    Post("/example", sample) ~> routes ~> check {
      response.status should be (StatusCodes.Conflict)
    }
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[SoftInstance] should be (SoftInstance(42, "test"))
    }
  }

  "delete item from repository" in {
    Delete("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.NoContent)
    }
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.NotFound)
    }
  }
}
