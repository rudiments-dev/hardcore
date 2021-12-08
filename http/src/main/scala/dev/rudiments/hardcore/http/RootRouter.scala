package dev.rudiments.hardcore.http

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.{Directives, Route}
import akka.http.scaladsl.server.Directives._
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
  private val prefixExists = config.hasPath(RootRouter.prefixPath)
  private val prefix = if(prefixExists) Some(config.getString(RootRouter.prefixPath)) else None

  override val routes: Route =
    CorsDirectives.cors(CorsSettings(config.getConfig(RootRouter.rootPath))) {
      prefix match {
        case Some(pre)  => Directives.pathPrefix(pre) { routers.map(_.routes).reduce(_ ~ _) }
        case None       => routers.map(_.routes).reduce(_ ~ _)
      }
    }

  def bind(): Done = {
    Http().newServerAt(
      config.getString(RootRouter.hostPath),
      config.getInt(RootRouter.portPath)
    ).bind(routes).onComplete {
      case Success(b) => logger.info("Bound http:/{}{}", b.localAddress.toString, prefix.map("/" + _).getOrElse(""))
      case Failure(e) => throw e
    }
    Done
  }
}

object RootRouter {
  val rootPath = "http"
  val prefixPath = s"$rootPath.prefix"
  val hostPath = s"$rootPath.host"
  val portPath = s"$rootPath.port"
}
