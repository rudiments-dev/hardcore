package dev.rudiments.hardcore.types

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import dev.rudiments.hardcore.data.DataMemoryAdapter
import dev.rudiments.hardcore.data.ReadOnly._
import dev.rudiments.hardcore.http.CirceSupport._
import dev.rudiments.hardcore.types.MyEnum.{One, Red, Two}
import dev.rudiments.types.registry.module.TypeHttpPort
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class TypePortSpec extends WordSpec with Matchers with ScalatestRouteTest {
  private case class Example(
    id: Long = Defaults.long,
    name: String,
    enum: MyEnum
  ) extends DTO

  private implicit val actorSystem: ActorSystem = ActorSystem()
  private implicit val t: HardType[Type] = HardType[Type]
  private val repo: DataMemoryAdapter[Type] = new DataMemoryAdapter[Type]

  private val router: TypeHttpPort[Type, String] = new TypeHttpPort(
    "example",
    e => ID(e.name),
    repo
  )
  private val routes = Route.seal(router.routes)
  private val sample: Type = HardType[Example]
  private val id = ID(sample.name)

  "no element by ID" in {
    Get("/example/-1") ~> routes ~> check {
      response.status should be (StatusCodes.NotFound)
    }
  }

  "put item into repository" in {
    Post("/example", sample) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
      responseAs[Type] should be (sample)
    }
  }

  "update item in repository" in {
    Put("/example/Example", Type("Example", Map(
      "id" -> Field(RudimentTypes.Number, FieldFlag.WithDefault),
      "name" -> Field(RudimentTypes.Text, FieldFlag.Required)
    ))) ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Type] should be (Type("Example", Map(
        "id" -> Field(RudimentTypes.Number, FieldFlag.WithDefault),
        "name" -> Field(RudimentTypes.Text, FieldFlag.Required)
      )))
    }
  }

  "second create makes conflict" in {
    Post("/example", sample) ~> routes ~> check {
      response.status should be (StatusCodes.Conflict)
    }
    Get("/example/Example") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Type] should be (Type("Example", Map(
        "id" -> Field(RudimentTypes.Number, FieldFlag.WithDefault),
        "name" -> Field(RudimentTypes.Text, FieldFlag.Required)
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
      Post("/example", Type(s"Example-$i", Map(
        "id" -> Field(RudimentTypes.Number, FieldFlag.WithDefault),
        s"name-$i" -> Field(RudimentTypes.Text, FieldFlag.Required)
      ))) ~> routes ~> check {
        response.status should be (StatusCodes.Created)
      }
    }
    repo(Count()(t)) should be (Counted(10000))
    Get("/example/Example-24") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Type] should be (Type("Example-24", Map(
        "id" -> Field(RudimentTypes.Number, FieldFlag.WithDefault),
        "name-24" -> Field(RudimentTypes.Text, FieldFlag.Required)
      )))
    }
  }

  "endure 190.000 batch" in {
    Put("/example", (10001 to 200000).map(i => Type(s"Example-$i", Map(
      "id" -> Field(RudimentTypes.Number, FieldFlag.WithDefault),
      s"name-$i" -> Field(RudimentTypes.Text, FieldFlag.Required)
    )))) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
      repo(Count()(t)) should be (Counted(200000))
    }
    Get("/example/Example-10024") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Type] should be (Type("Example-10024", Map(
        "id" -> Field(RudimentTypes.Number, FieldFlag.WithDefault),
        "name-10024" -> Field(RudimentTypes.Text, FieldFlag.Required)
      )))
    }
  }

  "clear repository" in {
    Delete("/example") ~> routes ~> check {
      response.status should be (StatusCodes.NoContent)
      repo(Count()(t)) should be (Counted(0))
    }
  }

}
