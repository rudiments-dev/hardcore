package dev.rudiments.hardcore.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import dev.rudiments.hardcore._
import io.circe.Decoder

import scala.language.implicitConversions

class ScalaRouter(mem: Node)(implicit td: ThingDecoder) extends CirceSupport {
  val routes: Route = {
    path(Segments(1, 128) ~ Slash) { segments =>
        get {
          mem.navigate(segments) match {
            case (_, Unmatched) => NotExist
            case (_, l) => mem ?? l
          }
        }
    } ~ path(Segments(1, 128)) { segments =>
      mem.navigate(segments) match {
        case (_, Unmatched) => NotExist
        case (node, location) =>
          implicit val d: Decoder[Data] = td.decoder(node.leafIs).map(_.asInstanceOf[Data])
          parameterMultiMap { params =>
            get {
              if (params.contains("structure")) {
                mem ?* location
              } else {
                mem ? location
              }
            } ~ delete {
              mem -= location
            } ~ entity(as[Data]) { data =>
              post {
                mem += location -> data
              } ~ put {
                mem *= location -> data
              }
            }
          }
      }
    } ~ pathEnd {
      parameterMultiMap { params =>
        get {
          if (params.contains("structure")) {
            mem ?* Root
          } else {
            mem ? Root
          }
        }
      }
    } ~ pathSingleSlash {
      get {
        mem ?? Root
      }
    }
  }

  private implicit def responseWith(event: Out): StandardRoute = event match {
    case Created(value) =>
      complete(StatusCodes.Created, value)
    case Readen(value) =>
      complete(StatusCodes.OK, value)
    case Updated(_, newValue) =>
      complete(StatusCodes.OK, newValue)
    case Deleted(_) =>
      complete(StatusCodes.NoContent)
    case Found(_, values) =>
      val node = Node.fromMap(values)
      complete(StatusCodes.OK, node.asInstanceOf[Thing])
    case NotExist =>
      complete(StatusCodes.NotFound)
    case _: NotFound =>
      complete(StatusCodes.NotFound)
    case AlreadyExist(_) =>
      complete(StatusCodes.Conflict)
    case out: CRUD.O =>
      complete(StatusCodes.OK, out.asInstanceOf[Thing])

    case _: Error =>
      complete(StatusCodes.InternalServerError)
    case _ =>
      complete(StatusCodes.InternalServerError)
  }

  def seal(): Route = this.seal("")

  def seal(prefix: String): Route = if(prefix != ""){
    Route.seal(pathPrefix(prefix) { routes })
  } else {
    Route.seal(routes)
  }
}
