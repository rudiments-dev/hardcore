package dev.rudiments.domain.registry

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import dev.rudiments.hardcore.http.RootRouter

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

object DomainRegistry extends App with LazyLogging {
  logger.info("Configuring application")

  val config = ConfigFactory.load()

  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val ec: ExecutionContext = actorSystem.dispatcher
  implicit val mat: ActorMaterializer = ActorMaterializer()

  val module = new DomainModule

  val router: RootRouter = new RootRouter(config, module.http)

  logger.info("Starting application")

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
