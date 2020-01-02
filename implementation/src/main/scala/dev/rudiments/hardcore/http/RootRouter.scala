package dev.rudiments.hardcore.http

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import akka.stream.Materializer
import ch.megard.akka.http.cors.scaladsl.CorsDirectives
import ch.megard.akka.http.cors.scaladsl.settings.CorsSettings
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

class RootRouter(
  config: Config,
  routers: Router*
)(implicit
  actorSystem: ActorSystem,
  mat: Materializer
) extends Router with StrictLogging {

  private implicit val ec: ExecutionContext = actorSystem.getDispatcher
  private val prefix = config.getString("http.prefix")

  override val routes: Route =
    CorsDirectives.cors(CorsSettings(config)) {
      PrefixRouter(prefix, routers: _*).routes
    }

  def bind(): Done = {
    Http().bindAndHandle(
      routes,
      config.getString("http.host"),
      config.getInt("http.port")
    ).onComplete {
      case Success(b) => logger.info("Bound http://{}/{}", b.localAddress.toString, prefix)
      case Failure(e) => throw e
    }
    Done
  }
}
