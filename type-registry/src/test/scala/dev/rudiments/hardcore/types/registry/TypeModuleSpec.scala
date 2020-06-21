package dev.rudiments.hardcore.types.registry

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.data.ReadOnly.{Count, Counted}
import dev.rudiments.data.SoftModule
import dev.rudiments.hardcore.types._
import io.circe.{Decoder, Encoder}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class TypeModuleSpec extends WordSpec with Matchers with ScalatestRouteTest with FailFastCirceSupport {
  private case class Example(
    id: Long,
    name: String
  ) extends DTO

  private implicit val actorSystem: ActorSystem = ActorSystem()
  private val module = SoftModule("types", "name")(HardType[Type])
  private implicit val t: Type = module.context.t

  private implicit val en: Encoder[Instance] = module.context.encoder
  private implicit val de: Decoder[Instance] = module.context.decoder
  private val routes = Route.seal(module.port.routes)

  private val sample: Instance = t.softFromHard(HardType[Example])

  "no element by ID" in {
    Get("/example/-1") ~> routes ~> check {
      response.status should be (StatusCodes.NotFound)
    }
  }

  "put item into repository" in {
    Post("/example", sample) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
      responseAs[Instance] should be (sample)
    }
  }

  "update item in repository" in {
    Put("/example/Example", SoftInstance("Example", Map(
      "id" -> Field(ScalaTypes.ScalaLong, FieldFlag.WithDefault),
      "name" -> Field(ScalaTypes.ScalaString, FieldFlag.Required)
    )).asInstanceOf[Instance]) ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (SoftInstance("Example", Map(
        "id" -> Field(ScalaTypes.ScalaLong, FieldFlag.WithDefault),
        "name" -> Field(ScalaTypes.ScalaString, FieldFlag.Required)
      )))
    }
  }

  "second create makes conflict" in {
    Post("/example", sample) ~> routes ~> check {
      response.status should be (StatusCodes.Conflict)
    }
    Get("/example/Example") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (SoftInstance("Example", Map(
        "id" -> Field(ScalaTypes.ScalaLong, FieldFlag.WithDefault),
        "name" -> Field(ScalaTypes.ScalaString, FieldFlag.Required)
      )))
    }
  }

  "delete item from repository" in {
    Delete("/example/Example") ~> routes ~> check {
      response.status should be (StatusCodes.NoContent)
    }
    Get("/example/Example") ~> routes ~> check {
      response.status should be (StatusCodes.NotFound)
    }
  }

  "endure 10.000 records" in {
    (1 to 10000).foreach { i =>
      Post("/example", SoftInstance(s"Example-$i", Map(
        "id" -> Field(ScalaTypes.ScalaLong, FieldFlag.WithDefault),
        s"name-$i" -> Field(ScalaTypes.ScalaString, FieldFlag.Required)
      )).asInstanceOf[Instance]) ~> routes ~> check {
        response.status should be (StatusCodes.Created)
      }
    }
    module.context.adapter(Count).merge should be (Counted(10000))
    Get("/example/Example-24") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (SoftInstance("Example-24", Map(
        "id" -> Field(ScalaTypes.ScalaLong, FieldFlag.WithDefault),
        "name-24" -> Field(ScalaTypes.ScalaString, FieldFlag.Required)
      )))
    }
  }

  "endure 190.000 batch" in {
    Post("/example", (10001 to 200000).map(i => SoftInstance(s"Example-$i", Map(
      "id" -> Field(ScalaTypes.ScalaLong, FieldFlag.WithDefault),
      s"name-$i" -> Field(ScalaTypes.ScalaString, FieldFlag.Required)
    )).asInstanceOf[Instance])) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
      module.context.adapter(Count).merge should be (Counted(200000))
    }
    Get("/example/Example-10024") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (SoftInstance("Example-10024", Map(
        "id" -> Field(ScalaTypes.ScalaLong, FieldFlag.WithDefault),
        "name-10024" -> Field(ScalaTypes.ScalaString, FieldFlag.Required)
      )))
    }
  }

  "clear repository" in {
    Delete("/example") ~> routes ~> check {
      response.status should be (StatusCodes.NoContent)
      module.context.adapter(Count).merge should be (Counted(0))
    }
  }

}
