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

import java.lang.management.ManagementFactory
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

class RootRouter(
  config: RootConfig,
  routes: (String, Route)*
)(implicit actorSystem: ActorSystem) extends StrictLogging {
  private implicit val ec: ExecutionContext = actorSystem.getDispatcher
  val routers = new Memory()

  def route: Route = CorsDirectives.cors(config.cors) {
    if (config.prefix != "") {
      Directives.pathPrefix(config.prefix) {
        routes.map {
          case (p, router: Route) => Directives.pathPrefix(p) {
            router
          }
        }.reduce(_ ~ _)
      }
    } else {
      routes.map {
        case (p, router: Route) => Directives.pathPrefix(p) {
          router
        }
      }.reduce(_ ~ _)
    }
  }


  def bind(): Done = {
    Http().newServerAt(
      config.host,
      config.port
    ).bind(route).onComplete {
      case Success(b) => logger.info("Bound http:/{}/{} on {}", b.localAddress.toString, config.prefix, ManagementFactory.getRuntimeMXBean.getName)
      case Failure(e) =>
        actorSystem.terminate()
        throw e
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
    prefix: String = "",
    cors: CorsSettings
  )

  import com.typesafe.config.Config
  def config(c: Config): RootConfig = RootConfig(
    c.getString(hostPath),
    c.getInt(portPath),
    if(c.hasPath(prefixPath)) c.getString(prefixPath) else "",
    CorsSettings(c.getConfig(rootPath))
  )
}
