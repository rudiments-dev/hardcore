package dev.rudiments.hardcore.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import dev.rudiments.hardcore.Predicate.All
import dev.rudiments.hardcore._
import io.circe.Decoder

import scala.language.implicitConversions

class ScalaRouter(val ctx: Context) extends CirceSupport {
  implicit val de: Decoder[Data] = ThingDecoder.dataDecoder(
    Type(Field("a", Bool))
  )

  val routes: Route = {
    path("last-commit") {
      get {
        responseWith(NotExist)
      }
    } ~ path(Segment) { str =>
      val id = ID(str)
      get {
        ctx ? id
      } ~ delete {
        ctx -= id
      } ~ entity(as[Data]) { data =>
        post {
          ctx += id -> data
        } ~ put {
          ctx *= id -> data
        }
      }
    } ~ pathSingleSlash {
      get {
        ctx << Find(All)
      }
    }
  }

  private implicit def responseWith(event: Out): StandardRoute = event match {
    case Created(value) =>       complete(StatusCodes.Created, value)
    case Readen(value) =>        complete(StatusCodes.OK, value)
    case Updated(_, newValue) => complete(StatusCodes.OK, newValue)
    case Deleted(_) =>           complete(StatusCodes.NoContent)
    case Found(_, values) =>     complete(StatusCodes.OK, Node.fromMap(values))
    case NotExist =>             complete(StatusCodes.NotFound)
    case AlreadyExist(_) =>      complete(StatusCodes.Conflict)
    case out: CRUD.O =>          complete(StatusCodes.OK, out)

    case _: Error =>             complete(StatusCodes.InternalServerError)
    case _ =>                    complete(StatusCodes.InternalServerError)
  }

  def seal(prefix: String): Route = Route.seal(pathPrefix(prefix) { routes })
}
