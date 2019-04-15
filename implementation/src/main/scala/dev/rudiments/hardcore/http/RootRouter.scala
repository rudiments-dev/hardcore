package dev.rudiments.hardcore.http

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive0, Route}
import akka.stream.Materializer
import ch.megard.akka.http.cors.scaladsl.CorsDirectives
import ch.megard.akka.http.cors.scaladsl.settings.CorsSettings
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

trait Router {
  val routes: Route
}
object Router {
  def apply(route: Route): Router = new Router {
    override val routes: Route = route
  }
}

class RootRouter(config: Config, routers: Router*)
  (implicit actorSystem: ActorSystem, mat: Materializer) extends Router with StrictLogging {

  val prefix: String = config.getString("http.prefix")
  val cors: Directive0 = CorsDirectives.cors(CorsSettings(config))

  private implicit val ec: ExecutionContext = actorSystem.getDispatcher

  override val routes: Route =
    cors {
      pathPrefix(prefix) {
        routers.map(_.routes).reduce(_ ~ _)
      }
    }

  def bind(): Done = {
    Http().bindAndHandle(
      routes,
      config.getString("http.host"),
      config.getInt("http.port")
    ).onComplete {
      case Success(b) => logger.info("Bound /{} on {}", prefix, b.localAddress)
      case Failure(e) => throw e
    }
    Done
  }
}



