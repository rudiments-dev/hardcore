package dev.rudiments.hardcore.sql

import java.sql.{Date, Time, Timestamp}
import java.time.{LocalDate, LocalDateTime, LocalTime}

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.dimafeng.testcontainers.{ForAllTestContainer, PostgreSQLContainer}
import com.typesafe.config.{Config, ConfigFactory}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.data._
import dev.rudiments.domain._
import dev.rudiments.hardcode.sql.schema.{Column, ColumnTypes, Table, TypedSchema}
import dev.rudiments.hardcode.sql.{SQLAdapter, SQLHttpPort}
import dev.rudiments.hardcore.All
import dev.rudiments.hardcore.http.{ThingDecoder, ThingEncoder}
import io.circe.{Decoder, Encoder}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}
import scalikejdbc.{DBSession, _}

import scala.collection.JavaConverters._

@RunWith(classOf[JUnitRunner])
class SQLDataHttpPortTest extends FlatSpec with Matchers with ScalatestRouteTest
  with FailFastCirceSupport with ForAllTestContainer {


  override def afterStart(): Unit = {
    super.afterStart()
    initConnectionPool(config)
  }

  override val container = PostgreSQLContainer(mountPostgresDataToTmpfs = true)

  private case class Example(
    id: Long,
    name: String,
    date: Date,
    timestamp: Timestamp,
    time: Time
  ) extends DTO

  private implicit val actorSystem: ActorSystem = ActorSystem()
  private implicit val domain: Domain = new Domain
  private implicit val t: Spec = domain.makeFromScala[Spec, Example] //todo fix primary keys
  private implicit val en: Encoder[Instance] = new ThingEncoder(domain).specEncoder(t)
  private implicit val de: Decoder[Instance] = new ThingDecoder(domain).specDecoder(t)

  def exampleInstance(id: Long, text: String): Instance = {
    t.fromProduct(domain, Example(
      id, text,
      Date.valueOf(LocalDate.now),
      Timestamp.valueOf(LocalDateTime.now),
      Time.valueOf(LocalTime.now)
    ))
  }

  private val sample = exampleInstance(42L, "sample")

  lazy val config: Config = ConfigFactory.parseMap(Map(
    "driver" -> container.driverClassName,
    "url" -> container.jdbcUrl,
    "user" -> container.username,
    "password" -> container.password,
    "schema" -> "test"
  ).asJava)

  def initConnectionPool(config: Config): String = {
    val driver = config.getString("driver")
    val url = config.getString("url")
    val user = config.getString("user")
    val password = config.getString("password")
    Class.forName(driver)
    ConnectionPool.add("test", url, user, password)
    config.getString("schema")
  }

  val repoFunction: DBSession => SQLAdapter = session => new SQLAdapter(schema = TypedSchema("test", Map(
    t -> Table("example", Seq(
      Column("id", ColumnTypes.INT, nullable = false, default = false, pk = true),
      Column("name", ColumnTypes.VARCHAR(255), nullable = false, default = false, pk = false),
      Column("date", ColumnTypes.DATE(100), nullable = false, default = false, pk = false),
      Column("timestamp", ColumnTypes.TIMESTAMP(255, timeZone = false), nullable = false, default = false, pk = false),
      Column("time", ColumnTypes.TIME(255, timeZone = false), nullable = false, default = false, pk = false)
    ))
  ), Set.empty), domain, session)

  lazy val pool = ConnectionPool.get("test")
  lazy val router: SQLHttpPort = new SQLHttpPort(
    "example",
    "id",
    i => ID(Seq(i.extract[Long]("id"))),
    pool,
    repoFunction
  )
  lazy val routes = Route.seal(router.routes)

  it should "should create table schema by name" in {
    using(DB(pool.borrow())) { db =>
      db.localTx { implicit session =>
        sql"CREATE SCHEMA test".execute().apply()

        sql"""CREATE TABLE test.example (
             |      id BIGINT PRIMARY KEY,
             |      name VARCHAR(255) NOT NULL,
             |      date DATE NOT NULL,
             |      timestamp TIMESTAMP NOT NULL,
             |      time TIME  NOT NULL
             |)""".stripMargin.execute().apply()

        sql"""SELECT * FROM test.example""".execute().apply()
      }
    }
  }

  it should "no element by ID" in {
    Get("/example/42") ~> routes ~> check {
      response.status should be(StatusCodes.NotFound)
    }
  }

  it should "put item into repository" in {
    Post("/example", sample) ~> routes ~> check {
      response.status should be(StatusCodes.Created)
      responseAs[Instance] should be(sample)
    }
    Get("/example/42") ~> routes ~> check {
      response.status should be(StatusCodes.OK)
      responseAs[Instance] should be(sample)
    }
  }

  it should "update item in repository" in {
    val toUpdate = exampleInstance(42L, "test")
    Put("/example/42", toUpdate) ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be(toUpdate)
    }
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be(toUpdate)
    }
  }

  it should "second POST with same item conflicts with existing" in {
    Post("/example", sample) ~> routes ~> check {
      response.status should be(StatusCodes.Conflict)
    }
  }

  it should "delete items from repository" in {
    Delete("/example/42") ~> routes ~> check {
      response.status should be(StatusCodes.NoContent)
    }
    Get("/example/42") ~> routes ~> check {
      response.status should be(StatusCodes.NotFound)
    }
  }

  it should "endure 100 records" in {
    (1 to 100).foreach { i =>
      val instance = exampleInstance(i, s"$i'th element")

      Post("/example", instance) ~> routes ~> check {
        response.status should be(StatusCodes.Created)
      }
    }
    using(DBSession(pool.borrow())) { session =>
      repoFunction(session)(Count(All)) should be(Counted(100))
    }
  }

  it should "should filter entities" in {
    Get("/example?id=lt:10") ~> routes ~> check {
      response.status should be(StatusCodes.OK)
      responseAs[Seq[Instance]].map(_.extract[Long]("id")) should be(1 until 10)
    }
  }

  it should "should replace all entities" in {
    val toUpdate = Seq(exampleInstance(1L, "replaced"))
    Put("/example", toUpdate) ~> routes ~> check {
      response.status should be(StatusCodes.OK)
    }
    Get("/example") ~> routes ~> check {
      response.status should be(StatusCodes.OK)
      responseAs[Seq[Instance]] should be(toUpdate)
    }
  }

  it should "clear repository" in {
    Delete("/example") ~> routes ~> check {
      response.status should be(StatusCodes.OK)
      using(DBSession(pool.borrow())) { session =>
        repoFunction(session)(Count(All)) should be(Counted(0))
      }
    }
  }

}
