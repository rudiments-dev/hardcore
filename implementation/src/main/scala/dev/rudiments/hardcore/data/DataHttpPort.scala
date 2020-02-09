package dev.rudiments.hardcore.data

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.hardcore.Port
import dev.rudiments.hardcore.data.Batch._
import dev.rudiments.hardcore.data.CRUD._
import dev.rudiments.hardcore.data.ReadOnly._
import dev.rudiments.hardcore.http._
import dev.rudiments.hardcore.types.ID._
import dev.rudiments.hardcore.types.{DTO, HardType, ID}
import io.circe.{Decoder, Encoder}

import scala.reflect.runtime.universe.TypeTag

class DataHttpPort[T <: DTO : HardType : Encoder : Decoder, K : TypeTag](
  prefix: String,
  identify: T => ID[T],
  override val f: DataSkill[T]
) extends Port[DataCommand[T], DataEvent[T]] with Router with FailFastCirceSupport {

  override val routes: Route = PrefixRouter(prefix,
    CompositeRouter(
      GetPort(FindAll[T](), f, responseWith),
      PostPort((value: T) => identify(value) match {
        case AutoID() => CreateAuto(value)
        case id => Create(id, value)
      }, f, responseWith),
      PostPort((batch: Seq[T]) => CreateAll(batch.groupBy(identify).mapValues(_.head)), f, responseWith),
      PutPort((batch: Seq[T]) => ReplaceAll(batch.groupBy(identify).mapValues(_.head)), f, responseWith),
      DeletePort(DeleteAll[T](), f, responseWith)
    ),
    IDRouter(
      IDPath[T, K],
      { id: ID[T] => GetPort(Find[T](id), f, responseWith) },
      { id: ID[T] => PutPort((value: T) => Update[T](id, value), f, responseWith) },
      { id: ID[T] => DeletePort(Delete[T](id), f, responseWith) }
    )
  ).routes

  def responseWith(event: DataEvent[T]): StandardRoute = event match {
    case Created(_, value) =>       complete(StatusCodes.Created, value)
    case Found(_, value) =>         complete(StatusCodes.OK, value)
    case FoundAll(values) =>        complete(StatusCodes.OK, values)
    case Updated(_, _, newValue) => complete(StatusCodes.OK, newValue)
    case Deleted(_, _) =>           complete(StatusCodes.NoContent)

    case AllCreated(_) =>           complete(StatusCodes.Created)
    case AllReplaced(_) =>          complete(StatusCodes.Created)
    case AllDeleted() =>            complete(StatusCodes.NoContent)

    case NotFound(_) =>             complete(StatusCodes.NotFound)
    case AlreadyExists(_, _) =>     complete(StatusCodes.Conflict)

    case _: Error =>                complete(StatusCodes.InternalServerError)
  }
}


