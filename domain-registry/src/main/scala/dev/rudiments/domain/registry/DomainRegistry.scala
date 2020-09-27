package dev.rudiments.domain.registry

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import dev.rudiments.data.DataHttpPort
import dev.rudiments.domain.{ID, ScalaTypes, SomeThing, Spec}
import dev.rudiments.hardcore.http.{RootRouter, ThingDecoder, ThingEncoder}

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

object DomainRegistry extends App with LazyLogging {
  logger.info("Configuring application")

  val config = ConfigFactory.load()
  val skill = new DomainSkill()

  private val http = new DataHttpPort(
    "domain",
    ScalaTypes.ScalaString,
    i => ID(Seq(i.extract[String]("name"))),
    skill
  )(
    skill.domain.makeFromScala[Spec, SomeThing],
    new ThingEncoder(skill.domain).abstractInstanceEncoder("Thing"),
    new ThingDecoder(skill.domain).abstractInstanceDecoder("Thing")
  )

  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val ec: ExecutionContext = actorSystem.dispatcher
  implicit val mat: ActorMaterializer = ActorMaterializer()

  val router: RootRouter = new RootRouter(config, http)

  logger.info("Starting application")
  run()

  def run(): Unit = {
    try {
      router.bind()
    } catch {
      case e: Throwable =>
        logger.error("Error while initializing app, shutdown", e)
        actorSystem.terminate().onComplete {
          case Success(t) =>
            logger.info("Terminated {} after error", t, e)
            sys.exit(-1)
          case Failure(err) =>
            logger.error("Termination failed with error {} after error", err, e)
            sys.exit(-2)
        }
    }
  }
}
