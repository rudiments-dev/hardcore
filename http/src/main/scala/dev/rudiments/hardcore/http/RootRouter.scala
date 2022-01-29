package dev.rudiments.hardcore.http

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.{Directives, Route}
import akka.http.scaladsl.server.Directives._
import ch.megard.akka.http.cors.scaladsl.CorsDirectives
import ch.megard.akka.http.cors.scaladsl.settings.CorsSettings
import com.typesafe.scalalogging.StrictLogging
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.http.RootRouter.RootConfig

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

class RootRouter(config: RootConfig)(implicit actorSystem: ActorSystem) extends Router with StrictLogging {
  private implicit val ec: ExecutionContext = actorSystem.getDispatcher
  val routers = new Memory(All, All) //TODO String, Router

  override def routes: Route =
    CorsDirectives.cors(config.cors) {
      Directives.pathPrefix(config.prefix) {
        routers(Find(All)) match {
          case Found(_, items) => items.collect {
            case (id, router: Router) => Directives.pathPrefix(id.k.toString) { router.routes }
          }.reduce(_ ~ _)
        }
      }
    }

  def bind(): Done = {
    Http().newServerAt(
      config.host,
      config.port
    ).bind(routes).onComplete {
      case Success(b) => logger.info("Bound http:/{}/{}", b.localAddress.toString, config.prefix)
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

  case class RootConfig(
    host: String,
    port: Int,
    prefix: String = "api",
    cors: CorsSettings
  )

  import com.typesafe.config.Config
  def config(c: Config): RootConfig = RootConfig(
    c.getString(hostPath),
    c.getInt(portPath),
    if(c.hasPath(prefixPath)) c.getString(prefixPath) else "api",
    CorsSettings(c.getConfig(rootPath))
  )
}
