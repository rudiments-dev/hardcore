package dev.rudiments.hardcode.sql

import dev.rudiments.hardcore.http.{Router, SoftDecoder, SoftEncoder}
import dev.rudiments.hardcore.types.{AutoID, ID, Instance, SoftInstance, Type}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.hardcore.{Port, Result, Skill}
import dev.rudiments.data.Batch._
import dev.rudiments.data.CRUD._
import dev.rudiments.data.DataEvent
import dev.rudiments.data.ReadOnly._
import dev.rudiments.hardcore.http._
import dev.rudiments.hardcore.http.query.Directives
import io.circe.{Decoder, Encoder}


class SQLDataHttpPort(
  prefix: String,
  idField: String,
  identify: Instance => ID,
  override val s: Skill[DataEvent]
)(implicit t: Type) extends Port(s) with Router with FailFastCirceSupport {

  implicit val en: Encoder[Instance] = SoftEncoder(t).contramap { case i: SoftInstance => i }
  implicit val de: Decoder[Instance] = SoftDecoder(t).map(_.asInstanceOf[Instance])

  override val routes: Route = PrefixRouter(prefix,
    CompositeRouter(
      GetDirectivePort(Directives.query(t), query => FindAll(query), s, responseWith),
      PostPort((value: Instance) => identify(value) match {
        case _: AutoID => CreateAuto(value)
        case id => Create(id, value)
      }, s, responseWith),
      PostPort((batch: Seq[Instance]) => CreateAll(batch.groupBy(identify).mapValues(_.head)), s, responseWith),
      PutPort((batch: Seq[Instance]) => ReplaceAll(batch.groupBy(identify).mapValues(_.head)), s, responseWith),
      DeletePort(DeleteAll, s, responseWith)
    ),
    IDRouter(
      IDPath(t.fields(idField).kind)(t),
      { id: ID => GetPort(Find(id), s, responseWith) },
      { id: ID => PutPort((value: Instance) => Update(id, value), s, responseWith) },
      { id: ID => DeletePort(Delete(id), s, responseWith) }
    )
  ).routes
  

  def responseWith(event: Result[DataEvent]): StandardRoute = event match {
    case Right(Created(_, value)) =>        complete(StatusCodes.Created, value)
    case Right(Found(_, value)) =>          complete(StatusCodes.OK, value)
    case Right(FoundAll(values)) =>         complete(StatusCodes.OK, values)
    case Right(Updated(_, _, newValue)) =>  complete(StatusCodes.OK, newValue)
    case Right(Deleted(_, _)) =>            complete(StatusCodes.NoContent)

    case Right(AllCreated(_)) =>            complete(StatusCodes.Created)
    case Right(AllReplaced(_)) =>           complete(StatusCodes.Created)
    case Right(AllDeleted) =>               complete(StatusCodes.NoContent)

    case Left(NotFound(_)) =>               complete(StatusCodes.NotFound)
    case Left(AlreadyExists(_, _)) =>       complete(StatusCodes.Conflict)

    case Left(_: Error) =>                  complete(StatusCodes.InternalServerError)
  }
}


