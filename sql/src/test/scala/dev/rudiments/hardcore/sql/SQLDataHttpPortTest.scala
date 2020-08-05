package dev.rudiments.hardcore.sql

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.dimafeng.testcontainers.scalatest.TestContainerForAll
import com.dimafeng.testcontainers.{ForAllTestContainer, PostgreSQLContainer}
import com.typesafe.config.{Config, ConfigFactory}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.data.ReadOnly.{Count, Counted}
import dev.rudiments.hardcode.sql.{SQLAdapter, SQLHttpPort}
import dev.rudiments.hardcode.sql.schema.{Column, ColumnTypes, Table, TypedSchema}
import dev.rudiments.hardcore.http.{InstanceDecoder, InstanceEncoder}
import dev.rudiments.types._
import io.circe.{Decoder, Encoder}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, FreeSpec, Matchers, WordSpec}
import scalikejdbc.{AutoSession, DBSession, _}

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
    name: String
  ) extends DTO

  private implicit val actorSystem: ActorSystem = ActorSystem()
  private implicit val typeSystem: TypeSystem = TypeSystem()
  private implicit val t: Type = typeSystem.asType[Example] //todo fix primary keys
  private implicit val en: Encoder[Instance] = new InstanceEncoder(typeSystem)(t)
  private implicit val de: Decoder[Instance] = new InstanceDecoder(typeSystem)(t)

  private val sample = t.construct(42L, "sample")
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
      Column("name", ColumnTypes.VARCHAR(255), nullable = false, default = false, pk = false)
    ))
  ), Set.empty), session)

  lazy val pool = ConnectionPool.get("test")
  lazy val router: SQLHttpPort = new SQLHttpPort(
    "example",
    "id",
    i => i.extractID("id"),
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
             |      name VARCHAR(255) NOT NULL
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
    Put("/example/42", Instance(42L, "test")) ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (Instance(42L, "test"))
    }
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (Instance(42L, "test"))
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
      Post("/example", Instance(i.toLong, s"$i'th element")) ~> routes ~> check {
        response.status should be(StatusCodes.Created)
      }
    }
    using(DBSession(pool.borrow())) { session =>
      repoFunction(session)(Count()).merge should be(Counted(100))
    }
    Get("/example/42") ~> routes ~> check {
      response.status should be(StatusCodes.OK)
      responseAs[Instance] should be(Instance(42L, "42'th element"))
    }
  }

  it should "should filter entities" in {
    Get("/example?query=id=less:10") ~> routes ~> check {
      response.status should be(StatusCodes.OK)
      responseAs[Seq[Instance]] should be(
        (1 until 10).map(i => Instance(i.toLong, s"$i'th element"))
      )
    }
  }

  it should "should update entity" in {
    val toUpdate = Instance(42L, "updated")
    Put("/example/42", toUpdate) ~> routes ~> check {
      response.status should be(StatusCodes.OK)
      responseAs[Instance] should be(toUpdate)
    }
  }

  it should "should replace all entities" in {
    val toUpdate = Seq(Instance(1L, "replaced"))
    Put("/example", toUpdate) ~> routes ~> check {
      response.status should be(StatusCodes.Created)
    }
    Get("/example") ~> routes ~> check {
      response.status should be(StatusCodes.OK)
      responseAs[Seq[Instance]] should be(toUpdate)
    }
  }

  it should "clear repository" in {
    Delete("/example") ~> routes ~> check {
      response.status should be(StatusCodes.NoContent)
      using(DBSession(pool.borrow())) { session =>
        repoFunction(session)(Count()).merge should be(Counted(0))
      }
    }
  }

}
