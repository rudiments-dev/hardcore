package dev.rudiments.types.registry.module

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.hardcore.Port
import dev.rudiments.hardcore.data.Batch._
import dev.rudiments.hardcore.data.CRUD._
import dev.rudiments.hardcore.data.{Batch, CRUD, DataCommand, DataEvent, DataSkill, ReadOnly}
import dev.rudiments.hardcore.data.ReadOnly._
import dev.rudiments.hardcore.http._
import dev.rudiments.hardcore.types.{DTO, HardType, ID}
import io.circe.{Decoder, Encoder}

import scala.collection.parallel
import scala.reflect.runtime.universe.TypeTag

class HttpMemoryModule[T <: DTO : HardType : Encoder : Decoder, K : TypeTag](
  prefix: String,
  identify: T => ID[T]
) extends Port[DataCommand[T], DataEvent[T]] with Router with FailFastCirceSupport {

  private implicit val content: parallel.mutable.ParMap[ID[T], T] = parallel.mutable.ParMap.empty[ID[T], T]
  val f: DataSkill[T] = ReadOnly.count

  override val routes: Route = PrefixRouter(prefix,
    CompositeRouter(
      GetPort(FindAll[T](), ReadOnly.findAll, responseWith),
      PostPort((value: T) => Create(identify(value), value), CRUD.create, responseWith),
      PutPort((batch: Seq[T]) => CreateAll(batch.groupBy(identify).mapValues(_.head)), Batch.createAll, responseWith),
      DeletePort(DeleteAll[T](), Batch.deleteAll, responseWith)
    ),
    IDRouter(
      IDPath[T, K],
      { id: ID[T] => GetPort(Find[T](id), ReadOnly.find, responseWith) },
      { id: ID[T] => PutPort((value: T) => Update[T](id, value), CRUD.update, responseWith) },
      { id: ID[T] => DeletePort(Delete[T](id), CRUD.delete, responseWith) }
    )
  ).routes

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


