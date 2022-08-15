package dev.rudiments.hardcore.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import dev.rudiments.hardcore.Predicate.{All, ThatNode, ThingsOnly}
import dev.rudiments.hardcore._
import io.circe.Decoder

import scala.language.implicitConversions

class ScalaRouter(mem: Node) extends CirceSupport {
  implicit val de: Decoder[Thing] = ThingDecoder.thingDecoder(mem.leafIs)

  val routes: Route = {
    path(Segments(1, 128) ~ Slash) { segments =>
      get {
        mem.decodeAndReadLocation(segments) match {
          case (_, Readen(m: Node)) => m << Find(ThatNode)
          case (l, t) =>
            NotImplemented
        }
      }
    } ~ path(Segments(1, 128)) { segments =>
      val (loc, ifErr) = mem.decodeAndReadLocation(segments)
      get {
        (loc, ifErr) match {
          case (Unmatched, _) => NotExist
          case (_, err: Error) => err
          case (l, _) => mem ? l
        }
      } ~ delete {
        (loc, ifErr) match {
          case (Unmatched, _) => NotExist
          case (_, err: Error) => err
          case (l, _) => mem -= l
        }
      } ~ entity(as[Thing]) { data =>
        post {
          (loc, ifErr) match {
            case (Unmatched, _) => NotExist
            case (_, err: Error) => err
            case (l, _) => mem += l -> data
          }
        } ~ put {
          (loc, ifErr) match {
            case (Unmatched, _) => NotExist
            case (_, err: Error) => err
            case (l, _) => mem *= l -> data
          }
        }
      }
      ifErr match {
        case err: Error => err
        case _ =>
          loc match {
            case Unmatched => NotExist
            case l =>
              get {
                mem ? l
              } ~ delete {
                mem -= l
              } ~ entity(as[Thing]) { data =>
                post {
                  mem += l -> data
                } ~ put {
                  mem *= l -> data
                }
              }
          }
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
      complete(StatusCodes.OK, node)
    case NotExist =>
      complete(StatusCodes.NotFound)
    case AlreadyExist(_) =>
      complete(StatusCodes.Conflict)
    case out: CRUD.O =>
      complete(StatusCodes.OK, out)

    case _: Error =>
      complete(StatusCodes.InternalServerError)
    case _ =>
      complete(StatusCodes.InternalServerError)
  }

  def seal(prefix: String): Route = Route.seal(pathPrefix(prefix) { routes })
}
