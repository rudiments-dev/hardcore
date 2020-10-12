package dev.rudiments.data

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.data.Batch._
import dev.rudiments.data.CRUD._
import dev.rudiments.data.ReadOnly._
import dev.rudiments.domain.{ID, Instance, Spec, Thing}
import dev.rudiments.hardcore.http._
import dev.rudiments.hardcore.http.query.Directives
import dev.rudiments.hardcore.{Event, PortWithoutDependency, Result, Skill}
import io.circe.{Decoder, Encoder}

class DataHttpPort(
  prefix: String,
  idField: Thing,
  identify: Instance => ID,
  override val s: Skill[Event],
  customRoutes: Seq[(String, Router)] = Seq.empty,
  customIdRoutes: Seq[(String, ID => Router)] = Seq.empty
)(implicit spec: Spec, en: Encoder[Instance], de: Decoder[Instance]) extends PortWithoutDependency(s) with Router with FailFastCirceSupport {
  import ports._

  override val routes: Route = PrefixRouter(prefix,
    CompositeRouter(
      GetDirectivePort(Directives.query(spec), FindAll.apply, s, responseWith),
      PostPort((value: Instance) => Create(identify(value), value), s, responseWith),
      PostPort((batch: Seq[Instance]) => CreateAll(batch.groupBy(identify).mapValues(_.head)), s, responseWith),
      PutPort((batch: Seq[Instance]) => Reconcile(batch.groupBy(identify).mapValues(_.head)), s, responseWith),
      DeletePort(DeleteAll(), s, responseWith),
      CompositeRouter(customRoutes.map { case (p, r) => PrefixRouter(p, r) } : _*)
    ),
    IDRouter(
      IDPath(idField),
      { id: ID => GetPort(Find(id), s, responseWith) },
      { id: ID => PutPort((value: Instance) => Update(id, value), s, responseWith) },
      { id: ID => DeletePort(Delete(id), s, responseWith) },
      { id: ID => CompositeRouter(customIdRoutes.map { case (p, r) => PrefixRouter(p, r(id)) } : _*) }
    )
  ).routes

  def responseWith(event: Result[Event]): StandardRoute = event match {
    case Right(Created(_, value)) =>        complete(StatusCodes.Created, value)
    case Right(Found(_, value)) =>          complete(StatusCodes.OK, value)
    case Right(FoundAll(values)) =>         complete(StatusCodes.OK, values)
    case Right(Updated(_, _, newValue)) =>  complete(StatusCodes.OK, newValue)
    case Right(Deleted(_, _)) =>            complete(StatusCodes.NoContent)

    case Right(Commit(_)) =>                complete(StatusCodes.OK) //TODO report amount of created/updated/deleted

    case Left(NotFound(_)) =>              complete(StatusCodes.NotFound)
    case Left(AlreadyExists(_, _)) =>      complete(StatusCodes.Conflict)

    case Left(_: Error) =>                 complete(StatusCodes.InternalServerError)
    case _ =>                                 complete(StatusCodes.InternalServerError)
  }
}


