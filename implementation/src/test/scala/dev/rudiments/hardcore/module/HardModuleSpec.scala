package dev.rudiments.hardcore.module

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.typesafe.config.ConfigFactory
import dev.rudiments.hardcore.data.MyEnum
import dev.rudiments.hardcore.data.MyEnum._
import dev.rudiments.hardcore.data.ReadOnly._
import dev.rudiments.hardcore.http.CirceSupport._
import dev.rudiments.hardcore.types.{DTO, Defaults, ID}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}

@RunWith(classOf[JUnitRunner])
class HardModuleSpec extends WordSpec with Matchers with ScalatestRouteTest {
  private case class Example(
    id: Long = Defaults.long,
    name: String,
    enum: MyEnum
  ) extends DTO

  private val module = new HardModule[Example, Long]("example", e => ID(e.id))
  private val app = new HardApp(ConfigFactory.load(), module)
  private val repo = module.adapter
  private val routes = app.routes
  private val sample = Example(42, "sample", Red)
  private val id = ID(sample.id)

  "no element by ID" in {
    Get("/api/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.NotFound)
    }
  }

  "put item into repository" in {
    Post("/api/example", sample) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
      responseAs[Example] should be (sample)
    }
  }

  "update item in repository" in {
    Put("/api/example/42", Example(42, "test", Red)) ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Example] should be (Example(42, "test", Red))
    }
  }

  "second create makes conflict" in {
    Post("/api/example", sample) ~> routes ~> check {
      response.status should be (StatusCodes.Conflict)
    }
    Get("/api/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Example] should be (Example(42, "test", Red))
    }
  }

  "delete item from repository" in {
    Delete("/api/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.NoContent)
    }
    Get("/api/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.NotFound)
    }
  }

  "endure 10.000 records" in {
    (1 to 10000).foreach { i =>
      Post("/api/example", Example(i, s"$i'th element", One)) ~> routes ~> check {
        response.status should be (StatusCodes.Created)
      }
    }
    repo(Count()) should be (Counted(10000))
    Get("/api/example/24") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Example] should be (Example(24, "24'th element", One))
    }
  }

  "endure 190.000 batch" in {
    Post("/api/example", (10001 to 200000).map(i => Example(i, s"$i'th element", Two))) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
      repo(Count()) should be (Counted(200000))
    }
    Get("/api/example/10024") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Example] should be (Example(10024, "10024'th element", Two))
    }
  }

  "clear repository" in {
    Delete("/api/example") ~> routes ~> check {
      response.status should be (StatusCodes.NoContent)
      repo(Count()) should be (Counted(0))
    }
  }

}
