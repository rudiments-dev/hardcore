package dev.rudiments.gates.h2

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import dev.rudiments.hardcore.ID
import io.circe.Json
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RegistryRouterSpec extends AnyWordSpec with SampleH2Gate with Matchers with ScalatestRouteTest {
  "should connect on correct credentials" in {
    gate(CheckConnection()) should be (ConnectionOk())
  }

  "should found schema by name" in {
    gate(InspectDB()) should be (InspectedDB(Set(ID[Schema, String]("HELLO2"), ID[Schema, String]("INFORMATION_SCHEMA"), ID[Schema, String]("PUBLIC"))))
  }

  import dev.rudiments.gates.h2.H2CirceSupport._
  private implicit val actorSystem: ActorSystem = ActorSystem()
  val router: RegistryRouter = new RegistryRouter(gate)
  private val routes = Route.seal(router.route)

  "/db/ find schemas" in {
    Get("/db/") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Json] should be (
        Json.arr(
          Json.obj("name" -> Json.fromString("PUBLIC")),
          Json.obj("name" -> Json.fromString("INFORMATION_SCHEMA")),
          Json.obj("name" -> Json.fromString("HELLO2"))
        )
      )
    }
  }

  "POST /db/inspect should inspect schema from router" in {
    Post("/db/inspect") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
    }
  }

  "/db/hello2 find tables" in {
    Get("/db/HELLO2/") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Json] should be (
        Json.arr(
          Json.obj(
            "name" -> Json.fromString("SAMPLE"),
            "columns" -> Json.arr(
              Json.obj(
                "name" -> Json.fromString("ID"),
                "type" -> Json.fromString("BIGINT"),
                "nullable" -> Json.fromBoolean(false),
                "default" -> Json.fromBoolean(true),
                "pk" -> Json.fromBoolean(true)
              ),
              Json.obj(
                "name" -> Json.fromString("NAME"),
                "type" -> Json.fromString("VARCHAR(255)"),
                "nullable" -> Json.fromBoolean(false),
                "default" -> Json.fromBoolean(false),
                "pk" -> Json.fromBoolean(false)
              ),
              Json.obj(
                "name" -> Json.fromString("COMMENT"),
                "type" -> Json.fromString("CLOB(2147483647, N)"),
                "nullable" -> Json.fromBoolean(true),
                "default" -> Json.fromBoolean(false),
                "pk" -> Json.fromBoolean(false)
              )
            )
          ),
          Json.obj(
            "name" -> Json.fromString("EXAMPLE"),
            "columns" -> Json.arr(
              Json.obj(
                "name" -> Json.fromString("ID"),
                "type" -> Json.fromString("BIGINT"),
                "nullable" -> Json.fromBoolean(false),
                "default" -> Json.fromBoolean(true),
                "pk" -> Json.fromBoolean(true)
              ),
              Json.obj(
                "name" -> Json.fromString("INT_COLUMN"),
                "type" -> Json.fromString("INT"),
                "nullable" -> Json.fromBoolean(true),
                "default" -> Json.fromBoolean(false),
                "pk" -> Json.fromBoolean(false)
              ),
              Json.obj(
                "name" -> Json.fromString("BOOL_COLUMN"),
                "type" -> Json.fromString("BOOLEAN"),
                "nullable" -> Json.fromBoolean(true),
                "default" -> Json.fromBoolean(false),
                "pk" -> Json.fromBoolean(false)
              ),
              Json.obj(
                "name" -> Json.fromString("TINYINT_COLUMN"),
                "type" -> Json.fromString("TINYINT"),
                "nullable" -> Json.fromBoolean(true),
                "default" -> Json.fromBoolean(false),
                "pk" -> Json.fromBoolean(false)
              ),
              Json.obj(
                "name" -> Json.fromString("SMALLINT_COLUMN"),
                "type" -> Json.fromString("SMALLINT"),
                "nullable" -> Json.fromBoolean(true),
                "default" -> Json.fromBoolean(false),
                "pk" -> Json.fromBoolean(false)
              ),
              Json.obj(
                "name" -> Json.fromString("BIGINT_COLUMN"),
                "type" -> Json.fromString("BIGINT"),
                "nullable" -> Json.fromBoolean(true),
                "default" -> Json.fromBoolean(false),
                "pk" -> Json.fromBoolean(false)
              ),
              Json.obj(
                "name" -> Json.fromString("DECIMAL_COLUMN"),
                "type" -> Json.fromString("DECIMAL(12)"),
                "nullable" -> Json.fromBoolean(true),
                "default" -> Json.fromBoolean(false),
                "pk" -> Json.fromBoolean(false)
              ),
              Json.obj(
                "name" -> Json.fromString("DOUBLE_COLUMN"),
                "type" -> Json.fromString("DOUBLE(17)"),
                "nullable" -> Json.fromBoolean(true),
                "default" -> Json.fromBoolean(false),
                "pk" -> Json.fromBoolean(false)
              ),
              Json.obj(
                "name" -> Json.fromString("REAL_COLUMN"),
                "type" -> Json.fromString("REAL(7)"),
                "nullable" -> Json.fromBoolean(true),
                "default" -> Json.fromBoolean(false),
                "pk" -> Json.fromBoolean(false)
              ),
              Json.obj(
                "name" -> Json.fromString("TIME_COLUMN"),
                "type" -> Json.fromString("TIME(8, true)"),
                "nullable" -> Json.fromBoolean(true),
                "default" -> Json.fromBoolean(false),
                "pk" -> Json.fromBoolean(false)
              ),
              Json.obj(
                "name" -> Json.fromString("DATE_COLUMN"),
                "type" -> Json.fromString("DATE(10)"),
                "nullable" -> Json.fromBoolean(true),
                "default" -> Json.fromBoolean(false),
                "pk" -> Json.fromBoolean(false)
              ),
              Json.obj(
                "name" -> Json.fromString("TIMESTAMP_COLUMN"),
                "type" -> Json.fromString("TIMESTAMP(26, true)"),
                "nullable" -> Json.fromBoolean(true),
                "default" -> Json.fromBoolean(false),
                "pk" -> Json.fromBoolean(false)
              ),
              Json.obj(
                "name" -> Json.fromString("BINARY_COLUMN"),
                "type" -> Json.fromString("BINARY(5)"),
                "nullable" -> Json.fromBoolean(true),
                "default" -> Json.fromBoolean(false),
                "pk" -> Json.fromBoolean(false)
              ),
              Json.obj(
                "name" -> Json.fromString("VARCHAR_COLUMN"),
                "type" -> Json.fromString("VARCHAR(67)"),
                "nullable" -> Json.fromBoolean(true),
                "default" -> Json.fromBoolean(false),
                "pk" -> Json.fromBoolean(false)
              ),
              Json.obj(
                "name" -> Json.fromString("CHAR_COLUMN"),
                "type" -> Json.fromString("CHAR(89)"),
                "nullable" -> Json.fromBoolean(true),
                "default" -> Json.fromBoolean(false),
                "pk" -> Json.fromBoolean(false)
              ),
              Json.obj(
                "name" -> Json.fromString("BLOB_COLUMN"),
                "type" -> Json.fromString("BLOB(10, N)"),
                "nullable" -> Json.fromBoolean(true),
                "default" -> Json.fromBoolean(false),
                "pk" -> Json.fromBoolean(false)
              ),
              Json.obj(
                "name" -> Json.fromString("CLOB_COLUMN"),
                "type" -> Json.fromString("CLOB(10, N)"),
                "nullable" -> Json.fromBoolean(true),
                "default" -> Json.fromBoolean(false),
                "pk" -> Json.fromBoolean(false)
              ),
              Json.obj(
                "name" -> Json.fromString("UUID_COLUMN"),
                "type" -> Json.fromString("UUID(16)"),
                "nullable" -> Json.fromBoolean(true),
                "default" -> Json.fromBoolean(false),
                "pk" -> Json.fromBoolean(false)
              ),
              Json.obj(
                "name" -> Json.fromString("ARRAY_COLUMN"),
                "type" -> Json.fromString("ARRAY(2147483647)"),
                "nullable" -> Json.fromBoolean(true),
                "default" -> Json.fromBoolean(false),
                "pk" -> Json.fromBoolean(false)
              ),
              Json.obj(
                "name" -> Json.fromString("ENUM_COLUMN"),
                "type" -> Json.fromString("ENUM(5)"),
                "nullable" -> Json.fromBoolean(true),
                "default" -> Json.fromBoolean(false),
                "pk" -> Json.fromBoolean(false)
              )
            )
          )
        )
      )
    }
  }

  "/db/INFORMATION_SCHEMA find tables" in {
    Get("/db/INFORMATION_SCHEMA/") ~> routes ~> check {
      val json = responseAs[Json]
      json.asArray.map(_.size) should be (Some(33))
      response.status should be(StatusCodes.OK)
    }
  }

  "/db/HELLO2/SAMPLE read table" in {
    Get("/db/HELLO2/SAMPLE") ~> routes ~> check {
      response.status should be(StatusCodes.OK)
      responseAs[Json] should be (Json.obj(
        "name" -> Json.fromString("SAMPLE"),
        "columns" -> Json.arr(
          Json.obj(
            "name" -> Json.fromString("ID"),
            "type" -> Json.fromString("BIGINT"),
            "nullable" -> Json.fromBoolean(false),
            "default" -> Json.fromBoolean(true),
            "pk" -> Json.fromBoolean(true)
          ),
          Json.obj(
            "name" -> Json.fromString("NAME"),
            "type" -> Json.fromString("VARCHAR(255)"),
            "nullable" -> Json.fromBoolean(false),
            "default" -> Json.fromBoolean(false),
            "pk" -> Json.fromBoolean(false)
          ),
          Json.obj(
            "name" -> Json.fromString("COMMENT"),
            "type" -> Json.fromString("CLOB(2147483647, N)"),
            "nullable" -> Json.fromBoolean(true),
            "default" -> Json.fromBoolean(false),
            "pk" -> Json.fromBoolean(false)
          )
        )
      ))
    }
  }

  "/db/HELLO2/PIECE available after table create" in {
    {
      import scalikejdbc._
      implicit val session: DBSession = AutoSession

      sql"""CREATE TABLE piece (
           |      id IDENTITY PRIMARY KEY,
           |      name VARCHAR(255) NOT NULL
           |)""".stripMargin.execute().apply()
      session.close()
    }

    Post("/db/inspect") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
    }

    Get("/db/HELLO2/PIECE") ~> routes ~> check {
      response.status should be(StatusCodes.OK)
      responseAs[Json] should be (Json.obj(
        "name" -> Json.fromString("PIECE"),
        "columns" -> Json.arr(
          Json.obj(
            "name" -> Json.fromString("ID"),
            "type" -> Json.fromString("BIGINT"),
            "nullable" -> Json.fromBoolean(false),
            "default" -> Json.fromBoolean(true),
            "pk" -> Json.fromBoolean(true)
          ),
          Json.obj(
            "name" -> Json.fromString("NAME"),
            "type" -> Json.fromString("VARCHAR(255)"),
            "nullable" -> Json.fromBoolean(false),
            "default" -> Json.fromBoolean(false),
            "pk" -> Json.fromBoolean(false)
          )
        )
      ))
    }
  }
}
