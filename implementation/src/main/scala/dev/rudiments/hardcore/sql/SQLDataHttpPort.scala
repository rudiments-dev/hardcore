package dev.rudiments.hardcore.sql


import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.hardcore.Port
import dev.rudiments.hardcore.data.soft.Batch._
import dev.rudiments.hardcore.data.soft.{DataCommand, DataEvent, DataSkill}
import dev.rudiments.hardcore.data.soft.SoftCRUD._
import dev.rudiments.hardcore.data.soft.ReadOnly._
import dev.rudiments.hardcore.http._
import dev.rudiments.hardcore.types.{AutoID, ID, Instance, SoftInstance, Type}
import io.circe.{Decoder, Encoder}

class SQLDataHttpPort(
  prefix: String,
  idField: String,
  identify: Instance => ID,
  override val f: DataSkill
)(implicit t: Type) extends Port[DataCommand, DataEvent] with Router with FailFastCirceSupport {

  implicit val en: Encoder[Instance] = SoftEncoder(t).contramap { case i: SoftInstance => i }
  implicit val de: Decoder[Instance] = SoftDecoder(t).map(_.asInstanceOf[Instance])

  override val routes: Route = PrefixRouter(prefix,
    CompositeRouter(
      GetPort(FindAll, f, responseWith),
      PostPort((value: Instance) => identify(value) match {
        case _: AutoID => CreateAuto(value)
        case id => Create(id, value)
      }, f, responseWith),
      PostPort((batch: Seq[Instance]) => CreateAll(batch.groupBy(identify).mapValues(_.head)), f, responseWith),
      PutPort((batch: Seq[Instance]) => ReplaceAll(batch.groupBy(identify).mapValues(_.head)), f, responseWith),
      DeletePort(DeleteAll, f, responseWith)
    ),
    IDRouter(
      IDPath(t.fields(idField).kind)(t),
      { id: ID => GetPort(Find(id), f, responseWith) },
      { id: ID => PutPort((value: Instance) => Update(id, value), f, responseWith) },
      { id: ID => DeletePort(Delete(id), f, responseWith) }
    )
  ).routes
  

  def responseWith(event: DataEvent): StandardRoute = event match {
    case Created(_, value) =>       complete(StatusCodes.Created, value)
    case Found(_, value) =>         complete(StatusCodes.OK, value)
    case FoundAll(values) =>        complete(StatusCodes.OK, values)
    case Updated(_, _, newValue) => complete(StatusCodes.OK, newValue)
    case Deleted(_, _) =>           complete(StatusCodes.NoContent)

    case AllCreated(_) =>           complete(StatusCodes.Created)
    case AllReplaced(_) =>          complete(StatusCodes.Created)
    case AllDeleted =>            complete(StatusCodes.NoContent)

    case NotFound(_) =>             complete(StatusCodes.NotFound)
    case AlreadyExists(_, _) =>     complete(StatusCodes.Conflict)

    case _: Error =>                complete(StatusCodes.InternalServerError)
  }
}


