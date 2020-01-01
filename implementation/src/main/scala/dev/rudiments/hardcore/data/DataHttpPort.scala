package dev.rudiments.hardcore.data

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{Directive1, Route, StandardRoute}
import akka.http.scaladsl.server.Directives._
import dev.rudiments.hardcore.Port
import dev.rudiments.hardcore.data.Batch._
import dev.rudiments.hardcore.data.CRUD._
import dev.rudiments.hardcore.data.ReadOnly._
import dev.rudiments.hardcore.types.{DTO, HardType, ID}
import dev.rudiments.hardcore.http.Router
import io.circe.{Decoder, Encoder}

class DataHttpPort[T <: DTO : HardType : Encoder : Decoder](
  prefix: String,
  idDirective: Directive1[ID[T]],
  identify: T => ID[T],
  override val f: DataSkill[T]
) extends Port[DataCommand[T], DataEvent[T]] with Router {

  override val routes: Route = pathPrefix(prefix) {
    pathRoute ~ idDirective { id =>
      idRoute(id)
    }
  }

  import dev.rudiments.hardcore.http.CirceSupport._

  def pathRoute: Route = pathEndOrSingleSlash {
    get {
      responseWith(f(FindAll[T]))
    } ~ post {
      entity(as[T]) { value =>
        responseWith(f(Create(identify(value), value)))
      }
    } ~ put {
      entity(as[List[T]]) { batch =>
        responseWith(f(CreateAll(batch.groupBy(identify).mapValues(_.head))))
      }
    } ~ delete {
      responseWith(f(DeleteAll[T]()))
    }
  }

  def idRoute(id: ID[T]): Route = pathEndOrSingleSlash {
    get {
      responseWith(f(Find(id)))
    } ~ put {
      entity(as[T]) { newValue =>
        responseWith(f(Update(id, newValue)))
      }
    } ~ delete {
      responseWith(f(Delete(id)))
    }
  }

  def responseWith(event: DataEvent[T]): StandardRoute = event match {
    case Created(_, value) =>       complete(StatusCodes.Created, value)
    case Found(_, value) =>         complete(StatusCodes.OK, value)
    case FoundAll(values) =>        complete(StatusCodes.OK, values)
    case Updated(_, _, newValue) => complete(StatusCodes.OK, newValue)
    case Deleted(_, _) =>           complete(StatusCodes.NoContent)

    case AllCreated(_) =>           complete(StatusCodes.Created)
    case AllDeleted() =>            complete(StatusCodes.NoContent)

    case NotFound(_) =>             complete(StatusCodes.NotFound)
    case AlreadyExists(_, _) =>     complete(StatusCodes.Conflict)

    case _: Error =>                complete(StatusCodes.InternalServerError)
  }
}


