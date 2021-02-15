package dev.rudiments.another

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.another.hardcore._
import org.junit.runner.RunWith
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.junit.JUnitRunner

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class DataHttpPortSpec extends AnyWordSpec with Matchers with ScalatestRouteTest with FailFastCirceSupport {
  private case class Example(
    id: Long,
    name: String
  )
  
  private implicit val actorSystem: ActorSystem = ActorSystem()
  private val state: State[Example] = new State()
  private val pipeline = new Pipeline[In, In, Tx] ({ rq => (rq, NoTx) })
  private val drainage = new Drainage[Out, Tx, Out]({ (out, _) => out })
  private val service = new Service(pipeline, state, drainage)

  import dev.rudiments.hardcore.http.CirceSupport._
  private val router: DataHttpPort[Example, In, Tx, Out, Long] = new DataHttpPort(
    "example",
    i => ID[Example](Seq(i.id)),
    service
  )

  private val routes = Route.seal(router.routes)
  private val sample = Example(42L, "sample")

  "no element by ID" in {
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.NotFound)
    }
  }

  "put item into repository" in {
    Post("/example", sample) ~> routes ~> check {
      response.status should be (StatusCodes.Created)
      responseAs[Example] should be (sample)
    }
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Example] should be (sample)
    }
  }

  "update item in repository" in {
    Put("/example/42", Example(42L, "test")) ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Example] should be (Example(42L, "test"))
    }
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Example] should be (Example(42L, "test"))
    }
  }

  "move item in repository" in {
    Put("/example/42", Example(24L, "test")) ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Example] should be (Example(24L, "test"))
    }
    Get("/example/24") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Example] should be (Example(24L, "test"))
    }
  }

  "second POST with same item conflicts with existing" in {
    Post("/example", Example(24L, "test")) ~> routes ~> check {
      response.status should be (StatusCodes.Conflict)
    }
    Get("/example") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Seq[Example]] should be (Seq(Example(24L, "test")))
    }
  }

  "delete items from repository" in {
    Delete("/example/24") ~> routes ~> check {
      response.status should be (StatusCodes.NoContent)
    }
    Get("/example/24") ~> routes ~> check {
      response.status should be (StatusCodes.NotFound)
    }
  }

  "endure 10.000 records" in {
    (1 to 10000).foreach { i =>
      Post("/example", Example(i.toLong, s"$i'th element")) ~> routes ~> check {
        response.status should be (StatusCodes.Created)
      }
    }
    state(Count(All)) should be (Counted(10000))
    Get("/example/42") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Example] should be (Example(42L, "42'th element"))
    }
  }

  "endure 190.000 batch" in {
    Post("/example", (10001 to 200000).map(i => Example(i.toLong, s"$i'th element"))) ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      state(Count(All)) should be (Counted(200000))
    }
    Get("/example/10042") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Example] should be (Example(10042L, "10042'th element"))
    }
  }

  "endure 100.000 replace" in {
    val batch = (200001 to 300000).map(i => (ID[Example](Seq(i.toLong)), Example(i.toLong, s"$i item"))).toMap
    val deleting = state(FindAll[Example](All)).asInstanceOf[FoundAll[Example]].content.values.map { it =>
      val id = ID[Example](Seq(it.id))
      id -> Deleted(id, it)
    }.toMap
    state(ReplaceAll[Example](batch)) should be (Commit(
      deleting ++ batch.map { case (k, v) => k -> Created[Example](k, v) }
    ))

    state(Count(All)) should be (Counted(100000))

    val rnd = new Random().nextInt(100000).toLong + 200000L
    state(Find(ID(Seq(rnd)))) should be (Found(
      ID(Seq(rnd)),
      Example(rnd, s"$rnd item")))
  }

  "clear repository" in {
    Delete("/example") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      state(Count(All)) should be (Counted(0))
    }
  }

}
