package work.unformed.hardcore.http

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive0, Route}
import akka.stream.Materializer
import ch.megard.akka.http.cors.scaladsl.CorsDirectives
import ch.megard.akka.http.cors.scaladsl.settings.CorsSettings
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

trait Router {
  val routes: Route
}

class RootRouter(config: Config, routers: Router*)
  (implicit actorSystem: ActorSystem, mat: Materializer) extends StrictLogging {

  val prefix: String = config.getString("http.prefix")
  val cors: Directive0 = CorsDirectives.cors(CorsSettings(config.getConfig("http.cors")))

  private implicit val ec: ExecutionContext = actorSystem.getDispatcher

  val routes: Route =
    cors {
      pathPrefix(prefix) {
        routers.map(_.routes).reduce(_ ~ _)
      }
    }

  def bind(): Future[Http.ServerBinding] = {
    val future = Http().bindAndHandle(
      routes,
      config.getString("http.host"),
      config.getInt("http.port")
    )

    future.onComplete {
      case Success(b) => logger.info("Bound on {}", b.localAddress)
      case Failure(e) => throw e
    }

    future
  }
}



