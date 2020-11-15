package dev.rudiments.hardcode.sql


import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.{Route, StandardRoute}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.data.Batch._
import dev.rudiments.data.CRUD._
import dev.rudiments.data.DataEvent
import dev.rudiments.data.ReadOnly._
import dev.rudiments.hardcore.http.query.{Directives, Query}
import dev.rudiments.hardcore.http._
import dev.rudiments.domain.{ID, Instance, Spec}
import dev.rudiments.hardcore.{Command, Failure, Port, Result, Skill, Success}
import io.circe.{Decoder, Encoder}
import scalikejdbc.{ConnectionPool, DBSession}

class SQLHttpPort
(
  prefix: String,
  idField: String,
  identify: Instance => ID,
  connectionPool: ConnectionPool,
  val s: DBSession => Skill[DataEvent]
)(implicit spec: Spec, en: Encoder[Instance], de: Decoder[Instance]) extends Port[Command, DataEvent] with Router with FailFastCirceSupport {
  import dev.rudiments.hardcore.http.HttpPorts.WithDependencies._

  override val routes: Route = PrefixRouter(prefix,
    CompositeRouter(
      GetDirectivePort[Query, FindAll, DataEvent, DBSession](Directives.query(spec), FindAll.apply, s, () => DBSession(connectionPool.borrow()), session => session.close(), responseWith),
      PostPort[Create, Instance, DataEvent, DBSession]((value: Instance) => Create(identify(value), value), s, () => DBSession(connectionPool.borrow()), session => session.close(), responseWith),
      PostPort[CreateAll, Seq[Instance], DataEvent, DBSession]((batch: Seq[Instance]) => CreateAll(batch.groupBy(identify).view.mapValues(_.head).toMap), s, () => DBSession(connectionPool.borrow()), session => session.close(), responseWith),
      PutPort[ReplaceAll, Seq[Instance], DataEvent, DBSession]((batch: Seq[Instance]) => ReplaceAll(batch.groupBy(identify).view.mapValues(_.head).toMap), s, () => DBSession(connectionPool.borrow()), session => session.close(), responseWith),
      DeletePort[DeleteAll, DataEvent, DBSession](DeleteAll(),  s, () => DBSession(connectionPool.borrow()), session => session.close(), responseWith),
    ),
    IDRouter(
      IDPath(spec.fields(idField).thing),
      { id: ID => GetPort[Find, DataEvent, DBSession](Find(id), s, () => DBSession(connectionPool.borrow()), session => session.close(), responseWith) },
      { id: ID => PutPort[Update, Instance, DataEvent, DBSession]((value: Instance) => Update(id, value), s, () => DBSession(connectionPool.borrow()), session => session.close(), responseWith) },
      { id: ID => DeletePort[Delete, DataEvent, DBSession](Delete(id), s, () => DBSession(connectionPool.borrow()), session => session.close(), responseWith) }
    )
  ).routes

  def responseWith(event: Result[DataEvent]): StandardRoute = event match {
    case Success(Created(_, value)) =>        complete(StatusCodes.Created, value)
    case Success(Found(_, value)) =>          complete(StatusCodes.OK, value)
    case Success(FoundAll(values)) =>         complete(StatusCodes.OK, values)
    case Success(Updated(_, _, newValue)) =>  complete(StatusCodes.OK, newValue)
    case Success(Deleted(_, _)) =>            complete(StatusCodes.NoContent)

    case Success(AllCreated(_)) =>            complete(StatusCodes.Created)
    case Success(AllReplaced(_)) =>           complete(StatusCodes.Created)
    case Success(AllDeleted()) =>             complete(StatusCodes.NoContent)

    case Failure(NotFound(_)) =>              complete(StatusCodes.NotFound)
    case Failure(AlreadyExists(_, _)) =>      complete(StatusCodes.Conflict)

    case Failure(_: Error) =>                 complete(StatusCodes.InternalServerError)
    case _ =>                                 complete(StatusCodes.InternalServerError)
  }
}
