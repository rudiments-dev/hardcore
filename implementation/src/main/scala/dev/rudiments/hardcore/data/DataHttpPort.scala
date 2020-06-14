package dev.rudiments.hardcore.data

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.hardcore.{Port, Result}
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
  override val s: DataSkill[T]
) extends Port[DataCommand[T], DataEvent[T]](s) with Router with FailFastCirceSupport {

  override val routes: Route = PrefixRouter(prefix,
    CompositeRouter(
      GetPort(FindAll[T](), s, responseWith),
      PostPort((value: T) => identify(value) match {
        case HardAutoID() => CreateAuto(value)
        case id => Create(id, value)
      }, s, responseWith),
      PostPort((batch: Seq[T]) => CreateAll(batch.groupBy(identify).mapValues(_.head)), s, responseWith),
      PutPort((batch: Seq[T]) => ReplaceAll(batch.groupBy(identify).mapValues(_.head)), s, responseWith),
      DeletePort(DeleteAll[T](), s, responseWith)
    ),
    HardIDRouter(
      IDPath[T, K],
      { id: HardID[T] => GetPort(Find[T](id), s, responseWith) },
      { id: HardID[T] => PutPort((value: T) => Update[T](id, value), s, responseWith) },
      { id: HardID[T] => DeletePort(Delete[T](id), s, responseWith) }
    )
  ).routes

  def responseWith(event: Result[DataEvent[T]]): StandardRoute = event match {
    case Right(Created(_, value)) =>        complete(StatusCodes.Created, value)
    case Right(Found(_, value)) =>          complete(StatusCodes.OK, value)
    case Right(FoundAll(values)) =>         complete(StatusCodes.OK, values)
    case Right(Updated(_, _, newValue)) =>  complete(StatusCodes.OK, newValue)
    case Right(Deleted(_, _)) =>            complete(StatusCodes.NoContent)

    case Right(AllCreated(_)) =>            complete(StatusCodes.Created)
    case Right(AllReplaced(_)) =>           complete(StatusCodes.Created)
    case Right(AllDeleted()) =>             complete(StatusCodes.NoContent)

    case Left(NotFound(_)) =>               complete(StatusCodes.NotFound)
    case Left(AlreadyExists(_, _)) =>       complete(StatusCodes.Conflict)

    case Left(_: Error) =>                  complete(StatusCodes.InternalServerError)
  }
}


