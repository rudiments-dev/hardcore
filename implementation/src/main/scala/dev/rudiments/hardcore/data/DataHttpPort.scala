package dev.rudiments.hardcore.data

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.hardcore.HardPort
import dev.rudiments.hardcore.data.Batch._
import dev.rudiments.hardcore.data.CRUD._
import dev.rudiments.hardcore.data.ReadOnly._
import dev.rudiments.hardcore.http._
import dev.rudiments.hardcore.types.HardID
import dev.rudiments.hardcore.types.HardID.HardAutoID
import io.circe.{Decoder, Encoder}

import scala.reflect.runtime.universe.TypeTag

class DataHttpPort[T : Encoder : Decoder, K : TypeTag](
  prefix: String,
  identify: T => HardID[T],
  override val h: DataSkill[T]
) extends HardPort[DataCommand[T], DataEvent[T]](h) with Router with FailFastCirceSupport {

  override val routes: Route = PrefixRouter(prefix,
    CompositeRouter(
      GetPort(FindAll[T](), h, responseWith),
      PostPort((value: T) => identify(value) match {
        case HardAutoID() => CreateAuto(value)
        case id => Create(id, value)
      }, h, responseWith),
      PostPort((batch: Seq[T]) => CreateAll(batch.groupBy(identify).mapValues(_.head)), h, responseWith),
      PutPort((batch: Seq[T]) => ReplaceAll(batch.groupBy(identify).mapValues(_.head)), h, responseWith),
      DeletePort(DeleteAll[T](), h, responseWith)
    ),
    HardIDRouter(
      IDPath[T, K],
      { id: HardID[T] => GetPort(Find[T](id), h, responseWith) },
      { id: HardID[T] => PutPort((value: T) => Update[T](id, value), h, responseWith) },
      { id: HardID[T] => DeletePort(Delete[T](id), h, responseWith) }
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


