package dev.rudiments.hardcore.sql

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import dev.rudiments.hardcore.http.{SoftDecoder, SoftEncoder}
import dev.rudiments.hardcore.types.{DTO, Defaults, HardType, SoftID, SoftInstance, Type}
import io.circe.{Decoder, Encoder}
import org.scalatest.{FlatSpec, Matchers, WordSpec}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest

class SQLDataHttpPortTest extends WordSpec with Matchers
  with ScalatestRouteTest {

  private case class Example(
                              id: Long = Defaults.long,
                              name: String
                            ) extends DTO

  private implicit val actorSystem: ActorSystem = ActorSystem()
  private implicit val t: Type = HardType[Example]
  private val repo: SQLAdapter = new SQLAdapter()

  private val router: SQLDataHttpPort = new SQLDataHttpPort(
    "example",
    "id",
    i => SoftID(t.extract(i, "id")),
    repo
  )
  private implicit val en: Encoder[SoftInstance] = SoftEncoder(t)
  private implicit val de: Decoder[SoftInstance] = SoftDecoder(t)

  private val routes = Route.seal(router.routes)
  private val sample = t.softFromHard(Example(42, "sample"))
  private val id = SoftID(t.extract(sample, "id"))


  "no element by ID" in {
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.NotFound)
    }
  }
}
