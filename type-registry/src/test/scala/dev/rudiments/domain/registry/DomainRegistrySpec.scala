package dev.rudiments.domain.registry

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.typesafe.config.ConfigFactory
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.another._
import io.circe.{Decoder, Encoder}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}


@RunWith(classOf[JUnitRunner])
class DomainRegistrySpec extends WordSpec with Matchers with ScalatestRouteTest with FailFastCirceSupport {
  private val config = ConfigFactory.load()
  private val skill = new DomainSkill()

  private implicit val en: Encoder[Instance] = new ThingEncoder(skill.domain).abstractInstanceEncoder("Thing")
  private implicit val de: Decoder[Instance] = new ThingDecoder(skill.domain).abstractInstanceDecoder("Thing")

  private val http = new HttpPort(
    "domain",
    ScalaTypes.ScalaString,
    i => ID(Seq(i.extract[String]("name"))),
    skill
  )(en, de)

  private val routes = Route.seal(new RootRouter(config, http).routes)

  "get all Thing in Domain" in {
    Get("/api/domain") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Seq[Instance]].size should be (26)
    }
  }

  "get Thing" in {
    Get("/api/domain/Thing") ~> routes ~> check {
      response.status should be (StatusCodes.OK)
      responseAs[Instance] should be (Instance(
        skill.domain.find[Spec]("Abstract"),
        Seq("Thing")
      ))
    }
  }

}
