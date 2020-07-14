package dev.rudiments.hardcore.sql

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.typesafe.config.{Config, ConfigFactory}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.data.ReadOnly.{Count, Counted}
import dev.rudiments.hardcode.sql.{SQLAdapter, SQLHttpPort}
import dev.rudiments.hardcode.sql.schema.{Column, ColumnTypes, Table, TypedSchema}
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
  private implicit val typeSystem: TypeSystem = new TypeSystem()
  private implicit val t: Type = ScalaType[Example] //todo fix primary keys
  private implicit val en: Encoder[Instance] = SoftEncoder(t)
  private implicit val de: Decoder[Instance] = SoftDecoder(t)

  private val sample = t.fromScala(Example(42, "sample"))

  val config: Config = ConfigFactory.parseMap(Map(
    "driver" -> "org.h2.Driver",
    "url" -> "jdbc:h2:mem:hi",
    "user" -> "user",
    "password" -> "pass",
    "schema" -> "hi"
  ).asJava)

  def initConnectionPool(config: Config): String = {
    val driver =    config.getString("driver")
    val url =       config.getString("url")
    val user =      config.getString("user")
    val password =  config.getString("password")
    Class.forName(driver)
    ConnectionPool.add("test", url, user, password)
    config.getString("schema")
  }
  initConnectionPool(config)

  private val repoFunction: DBSession => SQLAdapter = session => new SQLAdapter(schema = TypedSchema("hi", Map(
    t -> Table("example", Seq(
      Column("id", ColumnTypes.INT, nullable = false, default = false, pk = true),
      Column("name", ColumnTypes.VARCHAR(255), nullable = false, default = false, pk = false)
    ))
  ), Set.empty), session)
  val pool = ConnectionPool.get("test")

  private val router: SQLHttpPort = new SQLHttpPort(
    "example",
    "id",
    i => i.extractID("id"),
    pool,
    repoFunction
  )

  private val routes = Route.seal(router.routes)

  "should create table schema by name" in {
    using(DB(pool.borrow())) { db =>
      db.localTx { implicit session =>
        sql"CREATE SCHEMA hi".execute().apply()
        sql"SET SCHEMA hi".execute().apply()

        sql"""CREATE TABLE example (
             |      id IDENTITY PRIMARY KEY,
             |      name VARCHAR(255) NOT NULL,
             |)""".stripMargin.execute().apply()

        sql"""SELECT * FROM example""".execute().apply()
      }
    }
  }

  "no element by ID" in {
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.NotFound)
    }
  }

  "put item into repository" in {
    Post("/example", sample) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
      responseAs[Instance] should be (sample)
    }
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (sample)
    }
  }

  "update item in repository" in {
    Put("/example/42", SoftInstance(42L, "test")) ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (SoftInstance(42L, "test"))
    }
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (SoftInstance(42L, "test"))
    }
  }

  "second POST with same item conflicts with existing" in {
    Post("/example", sample) ~> routes ~> check {
      response.status should be (StatusCodes.Conflict)
    }
  }

  "delete items from repository" in {
    Delete("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.NoContent)
    }
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.NotFound)
    }
  }

  "endure 100 records" in {
    (1 to 100).foreach { i =>
      Post("/example", SoftInstance(i.toLong, s"$i'th element")) ~> routes ~> check {
        response.status should be (StatusCodes.Created)
      }
    }
    repo(Count()).merge should be (Counted(100))
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (SoftInstance(42L, "42'th element"))
    }
  }

  "endure 190.000 batch" in {
    Post("/example", (10001 to 200000).map(i => SoftInstance(i.toLong, s"$i'th element"))) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
      repo(Count()).merge should be (Counted(190100))
    }
    Get("/example/10042") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (SoftInstance(10042L, "10042'th element"))
    }
  }

  "should filter entities" in {
    Get("/example?query=id=less:10") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Seq[Instance]] should be (
        (1 until 10).map(i => SoftInstance(i.toLong, s"$i'th element"))
      )
    }
  }

  "should update entity" in {
    val toUpdate = SoftInstance(42L, "updated")
    Put("/example/42", toUpdate) ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (toUpdate)
    }
  }

  "should replace all entities" in {
    val toUpdate = Seq(SoftInstance(1L, "replaced"))
    Put("/example", toUpdate) ~> routes ~> check { response.status should be (StatusCodes.Created) }
    Get("/example") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Seq[Instance]] should be (toUpdate)
    }
  }



  "clear repository" in {
    Delete("/example") ~> routes ~> check {
      response.status should be (StatusCodes.NoContent)
      repo(Count()).merge should be (Counted(0))
    }
  }

}
