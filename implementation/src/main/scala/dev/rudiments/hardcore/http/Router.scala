package dev.rudiments.hardcore.http

import java.sql.Date

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive0, Directive1, Route}
import akka.stream.Materializer
import ch.megard.akka.http.cors.scaladsl.CorsDirectives
import ch.megard.akka.http.cors.scaladsl.settings.CorsSettings
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import dev.rudiments.hardcore.types.{DTO, ID, Type}

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
        routers.map(_.routes).foldRight(reject(): Route)(_ ~ _)
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

object IDPath {
  import scala.reflect.runtime.universe.{TypeTag, typeOf}
  def apply[A <: DTO : Type, K: TypeTag]: Directive1[ID[A]] = {
    if(typeOf[K] =:= typeOf[Long])   pathPrefix(LongNumber).map(l => ID[A, Long](l))
    else if(typeOf[K] =:= typeOf[Int])    pathPrefix(IntNumber).map(i => ID[A, Int](i))
    else if(typeOf[K] =:= typeOf[String]) pathPrefix(Segment).map(s => ID[A, String](s))
    else if(typeOf[K] =:= typeOf[Date])   pathPrefix(Segment).map(s => ID[A, Date](Date.valueOf(s)))
    else ??? //TODO enums
  }
}
