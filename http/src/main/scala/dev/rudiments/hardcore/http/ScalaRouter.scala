package dev.rudiments.hardcore.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import dev.rudiments.hardcore.Predicate.All
import dev.rudiments.hardcore._
import io.circe.Decoder

import scala.language.implicitConversions

class ScalaRouter(mem: Memory) extends CirceSupport {
  implicit val de: Decoder[Data] = mem.leafIs match {
    case t: Type => ThingDecoder.dataDecoder(t)
    case _ => throw new IllegalStateException("Only types supported for now")
  }

  val routes: Route = {
    path(Segment) { str =>
      val id = ID(str)
      get {
        mem ? id
      } ~ delete {
        mem -= id
      } ~ entity(as[Data]) { data =>
        post {
          mem += id -> data
        } ~ put {
          mem *= id -> data
        }
      }
    } ~ pathSingleSlash {
      get {
        mem << Find(All)
      }
    }
  }

  private implicit def responseWith(event: Out): StandardRoute = event match {
    case Created(value) =>       complete(StatusCodes.Created, value)
    case Readen(value) =>        complete(StatusCodes.OK, value)
    case Updated(_, newValue) => complete(StatusCodes.OK, newValue)
    case Deleted(_) =>           complete(StatusCodes.NoContent)
    case Found(_, values) =>     complete(StatusCodes.OK, Memory.fromMap(values))
    case NotExist =>             complete(StatusCodes.NotFound)
    case AlreadyExist(_) =>      complete(StatusCodes.Conflict)
    case out: CRUD.O =>          complete(StatusCodes.OK, out)

    case _: Error =>             complete(StatusCodes.InternalServerError)
    case _ =>                    complete(StatusCodes.InternalServerError)
  }

  def seal(prefix: String): Route = Route.seal(pathPrefix(prefix) { routes })
}
