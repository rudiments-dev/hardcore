package dev.rudiments.hardcore.module

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging
import dev.rudiments.hardcore.http.RootRouter

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

class HardApp(config: Config, modules: HardModule[_, _]*) extends LazyLogging {
  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val ec: ExecutionContext = actorSystem.dispatcher
  implicit val mat: ActorMaterializer = ActorMaterializer()

  val router: RootRouter = new RootRouter(config, modules.map(m => m.port):_*)
  val routes: Route = router.routes

  def init(): Unit = {
    try {
      router.bind()
    } catch {
      case e: Throwable =>
        logger.error("Error while initializing app, shutdown", e)
        actorSystem.terminate().onComplete {
          case Success(t) =>
            logger.info("Terminated {}", t)
            sys.exit(-1)
          case Failure(err) =>
            logger.error("Termination failed with error", err)
            sys.exit(-2)
        }
    }
  }
}
