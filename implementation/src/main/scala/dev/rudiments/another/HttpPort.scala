package dev.rudiments.another

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.another.Batch._
import dev.rudiments.another.CRUD._
import dev.rudiments.another.ReadOnly._
import dev.rudiments.data.DataEvent
import dev.rudiments.another.HttpPorts.DependencyLess._
import dev.rudiments.hardcore._
import io.circe.{Decoder, Encoder}

class HttpPort(
  prefix: String,
  idField: Plain,
  identify: Instance => ID,
  override val s: Skill[DataEvent],
  customRoutes: Seq[(String, Router)] = Seq.empty,
  customIdRoutes: Seq[(String, ID => Router)] = Seq.empty
)(implicit en: Encoder[Instance], de: Decoder[Instance]) extends PortWithoutDependency(s) with Router with FailFastCirceSupport {

  override val routes: Route = PrefixRouter(prefix,
    CompositeRouter(
      GetPort(FindAll(), s, responseWith),
      PostPort((value: Instance) => Create(identify(value), value), s, responseWith),
      PostPort((batch: Seq[Instance]) => CreateAll(batch.groupBy(identify).mapValues(_.head)), s, responseWith),
      PutPort((batch: Seq[Instance]) => ReplaceAll(batch.groupBy(identify).mapValues(_.head)), s, responseWith),
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


