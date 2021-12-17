package dev.rudiments.hardcore.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import dev.rudiments.hardcore._
import io.circe.{Decoder, Encoder, KeyEncoder}

import scala.reflect.runtime.universe.TypeTag

class ScalaRouter[T : TypeTag](
  override val path: Path,
  val id: Predicate,
  val agent: Agent
)(implicit en: Encoder[Thing], de: Decoder[Thing]) extends Router with CirceSupport {
  implicit val idEncoder: KeyEncoder[ID] = KeyEncoder.encodeKeyString.contramap(id => id.k.toString)
  implicit val valEncoder: Encoder[Map[ID, Thing]] = Encoder.encodeMap[ID, Thing]

  override val routes: Route = {
      plainId(id) { id =>
        get {
          responseWith(agent(Read(id)))
        } ~ delete {
          responseWith(agent(Delete(id)))
        } ~ entity(as[Thing]) { data =>
          post {
            responseWith(agent(Create(id, data)))
          } ~ put {
            responseWith(agent(Update(id, data)))
          }
        }
      } ~ get {
        responseWith(agent(Find(All)))
      }
    }

  def responseWith(event: Out): StandardRoute = event match {
    case Created(_, value: Thing) =>       complete(StatusCodes.Created, value)
    case Readen(_, value: Thing) =>        complete(StatusCodes.OK, value)
    case Updated(_, _, newValue: Thing) => complete(StatusCodes.OK, newValue)
    case Deleted(_, _) =>                 complete(StatusCodes.NoContent)

    case Found(_, values: Map[ID, Thing]) => complete(StatusCodes.OK, values)

    case NotFound(_) =>        complete(StatusCodes.NotFound)
    case AlreadyExist(_, _) => complete(StatusCodes.Conflict)

    case _: Error =>           complete(StatusCodes.InternalServerError)
    case _ =>                  complete(StatusCodes.InternalServerError)
  }
}
