package work.unformed.hardcore.sample

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import work.unformed.hardcore.http.{RootRouter, Router}

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

object Main extends App with LazyLogging {

  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val ec: ExecutionContext = actorSystem.dispatcher
  implicit val mat: ActorMaterializer = ActorMaterializer()

  try {
    val config = ConfigFactory.load()
    new RootRouter(config, new HealthRouter, AppContext.itemRouter).bind()
  } catch {
    case e: Throwable =>
      logger.error("Error while initializing app, shutdown", e)
      actorSystem.terminate().onComplete {
        case Success(t) => logger.info("Terminated {}", t)
        case Failure(err) =>
          logger.error("Termination failed with error", err)
          sys.exit(-1)
      }
  }
}

import akka.http.scaladsl.server.Directives._
class HealthRouter extends Router {
  override val routes: Route = path("health") {
    get {
      complete(StatusCodes.OK)
    }
  }
}