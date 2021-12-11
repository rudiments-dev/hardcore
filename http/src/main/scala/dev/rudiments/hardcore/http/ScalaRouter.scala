package dev.rudiments.hardcore.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import dev.rudiments.hardcore._
import io.circe.{Decoder, Encoder}

import scala.reflect.runtime.universe.TypeTag

class ScalaRouter[T : TypeTag : Encoder : Decoder](
  override val path: Path,
  val id: Predicate,
  val agent: Agent
) extends Router with CirceSupport {
  implicit val de: Decoder[Data] = implicitly[Decoder[T]].map(raw => Data.apply[T](raw))
  implicit val en: Encoder[Data] = implicitly[Encoder[T]].contramap(_.reconstruct[T]())
  implicit val idEncoder: Encoder[ID] = Encoder.encodeString.contramap(id => id.k.toString)

  override val routes: Route = {
      plainId(id) { id =>
        get {
          responseWith(agent(Read(id)))
        } ~ delete {
          responseWith(agent(Delete(id)))
        } ~ entity(as[Data]) { data =>
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
    case Created(_, value) =>        complete(StatusCodes.Created, value)
    case Readen(_, value) =>         complete(StatusCodes.OK, value)
    case Updated(_, _, newValue) =>  complete(StatusCodes.OK, newValue)
    case Deleted(_, _) =>            complete(StatusCodes.NoContent)
    case Found(_, values) =>         complete(StatusCodes.OK, values.values)

    case NotFound(_) =>              complete(StatusCodes.NotFound)
    case AlreadyExist(_, _) =>       complete(StatusCodes.Conflict)

    case _: Error =>                 complete(StatusCodes.InternalServerError)
    case _ =>                        complete(StatusCodes.InternalServerError)
  }
}
