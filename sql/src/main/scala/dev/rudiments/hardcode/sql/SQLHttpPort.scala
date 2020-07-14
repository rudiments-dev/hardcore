package dev.rudiments.hardcode.sql

import java.sql.Connection

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.{Route, StandardRoute}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.data.Batch.{AllCreated, AllDeleted, AllReplaced, CreateAll, DeleteAll, ReplaceAll}
import dev.rudiments.data.CRUD.{AlreadyExists, Create, Created, Delete, Deleted, Update, Updated}
import dev.rudiments.data.DataEvent
import dev.rudiments.data.ReadOnly.{Find, FindAll, Found, FoundAll, NotFound}
import dev.rudiments.hardcore.http.{CompositeRouter, DeletePort, GetDirectivePort, GetPort, IDPath, IDRouter, PostPort, PrefixRouter, PutPort, Router}
import dev.rudiments.hardcore.http.query.Directives
import dev.rudiments.hardcore.types.{ID, Instance, Type}
import dev.rudiments.hardcore.{Failure, PortWithResource, Result, Skill, Success}
import io.circe.{Decoder, Encoder}
import scalikejdbc.{ConnectionPool, DB, DBConnection, DBSession}
import scalikejdbc._

class SQLHttpPort
(
  prefix: String,
  idField: String,
  identify: Instance => ID,
  connectionPool: ConnectionPool,
  override val s: DBSession => Skill[DataEvent]
)(implicit t: Type, en: Encoder[Instance], de: Decoder[Instance]) extends PortWithResource(s) with Router with FailFastCirceSupport {

  override val routes: Route = PrefixRouter(prefix,
    CompositeRouter(
      GetDirectivePort(Directives.query(t), FindAll.apply, using(DB(connectionPool.borrow())){db => db.localTx(s)}, responseWith),
      PostPort((value: Instance) => Create(identify(value), value), using(DB(connectionPool.borrow())){db => db.localTx(s)}, responseWith),
      PostPort((batch: Seq[Instance]) => CreateAll(batch.groupBy(identify).mapValues(_.head)), using(DB(connectionPool.borrow())){db => db.localTx(s)}, responseWith),
      PutPort((batch: Seq[Instance]) => ReplaceAll(batch.groupBy(identify).mapValues(_.head)), using(DB(connectionPool.borrow())){db => db.localTx(s)}, responseWith),
      DeletePort(DeleteAll(), using(DB(connectionPool.borrow())){db => db.localTx(s)}, responseWith),
    ),
    IDRouter(
      IDPath(t.fields(idField).kind)(t),
      { id: ID => GetPort(Find(id), using(DB(connectionPool.borrow())){db => db.localTx(s)}, responseWith) },
      { id: ID => PutPort((value: Instance) => Update(id, value), using(DB(connectionPool.borrow())){db => db.localTx(s)}, responseWith) },
      { id: ID => DeletePort(Delete(id), using(DB(connectionPool.borrow())){db => db.localTx(s)}, responseWith) }
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
