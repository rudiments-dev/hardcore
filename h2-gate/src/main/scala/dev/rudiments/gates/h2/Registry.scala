package dev.rudiments.gates.h2

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

object Registry extends scala.App with StrictLogging {
  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val ec: ExecutionContext = actorSystem.dispatcher

  try {
    val config = ConfigFactory.load().getConfig("db")
    val gate = new H2Gate(H2Config(config).initConnectionPool())

    import dev.rudiments.gates.h2.H2CirceSupport._
    val router = new RegistryRouter(gate)
    bind(router.route)

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

  def bind(routes: Route): Done = {
    Http().bindAndHandle(
      routes,
      "localhost",
      8080
    ).onComplete {
      case Success(b) => logger.info("Bound http:/{}", b.localAddress.toString)
      case Failure(e) => throw e
    }
    Done
  }
}
