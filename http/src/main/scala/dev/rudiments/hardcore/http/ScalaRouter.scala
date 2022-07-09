package dev.rudiments.hardcore.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import dev.rudiments.hardcore.Memory.MemoryOps
import dev.rudiments.hardcore.Predicate.All
import dev.rudiments.hardcore._
import io.circe.Decoder

import scala.language.implicitConversions

class ScalaRouter(implicit val mount: Memory) extends CirceSupport {
  private implicit val de: Decoder[Data] = ThingDecoder.dataDecoder(
    Type(Field("a", Bool))
  )

  val routes: Route = {
    path(Segment) { str =>
      val id = ID(str)
      get {
        id.?
      } ~ delete {
        id.-=
      } ~ entity(as[Data]) { data =>
        post {
          id += data
        } ~ put {
          id *= data
        }
      }
    } ~ pathSingleSlash {
      get {
        mount << Find(All)
      }
    }
  }

  private implicit def responseWith(event: Out): StandardRoute = event match {
    case Created(value: Data) =>       complete(StatusCodes.Created, value)
    case Readen(value: Data) =>        complete(StatusCodes.OK, value)
    case Updated(_, newValue: Data) => complete(StatusCodes.OK, newValue)
    case Deleted(_) =>                 complete(StatusCodes.NoContent)

    case Found(_, values: Map[Location, Data]) => complete(StatusCodes.OK, Node.fromMap(values))

    case NotExist =>        complete(StatusCodes.NotFound)
    case AlreadyExist(_) => complete(StatusCodes.Conflict)

    case _: Error =>           complete(StatusCodes.InternalServerError)
    case _ =>                  complete(StatusCodes.InternalServerError)
  }

  def seal(prefix: String): Route = Route.seal(pathPrefix(prefix) { routes })
}
