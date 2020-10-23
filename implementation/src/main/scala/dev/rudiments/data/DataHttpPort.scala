package dev.rudiments.data

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.domain.{ID, Instance, Spec, Thing}
import dev.rudiments.hardcore.All
import dev.rudiments.hardcore.http._
import dev.rudiments.hardcore.http.query.Directives
import dev.rudiments.hardcore.{Message, PortWithoutDependency, Skill}
import io.circe.{Decoder, Encoder}

class DataHttpPort(
  prefix: String,
  idField: Thing,
  identify: Instance => ID,
  override val s: Skill,
  customRoutes: Seq[(String, Router)] = Seq.empty,
  customIdRoutes: Seq[(String, ID => Router)] = Seq.empty
)(implicit spec: Spec, en: Encoder[Instance], de: Decoder[Instance]) extends PortWithoutDependency(s) with Router with FailFastCirceSupport {
  import ports._

  override val routes: Route = PrefixRouter(prefix,
    CompositeRouter(
      GetPort(FindAll(All), s, responseWith),
      PostPort((value: Instance) => Create(identify(value), value), s, responseWith),
      PostPort((batch: Seq[Instance]) => CreateAll(batch.groupBy(identify).mapValues(_.head)), s, responseWith),
      PutPort((batch: Seq[Instance]) => Reconcile(batch.groupBy(identify).mapValues(_.head)), s, responseWith),
      DeletePort(DeleteUsing(All), s, responseWith),
      CompositeRouter(customRoutes.map { case (p, r) => PrefixRouter(p, r) } : _*)
    ),
    IDRouter(
      IDPath(idField),
      { id: ID => GetPort(Find(id), s, responseWith) },
      { id: ID => PutPort((value: Instance) => {
        val valueId = identify(value)
        if(id == valueId) {
          Update(id, value)
        } else {
          Move(id, valueId, value)
        }
      }, s, responseWith) }, // TODO handle Move if ID changed
      { id: ID => DeletePort(Delete(id), s, responseWith) },
      { id: ID => CompositeRouter(customIdRoutes.map { case (p, r) => PrefixRouter(p, r(id)) } : _*) }
    )
  ).routes

  def responseWith(event: Message): StandardRoute = event match {
    case Created(_, value) =>        complete(StatusCodes.Created, value)
    case Found(_, value) =>          complete(StatusCodes.OK, value)
    case FoundAll(values) =>         complete(StatusCodes.OK, values)
    case Updated(_, _, newValue) =>  complete(StatusCodes.OK, newValue)
    case Moved(_, _, _, newValue) => complete(StatusCodes.OK, newValue)
    case Deleted(_, _) =>            complete(StatusCodes.NoContent)

    case Commit(_) =>                complete(StatusCodes.OK) //TODO report amount of created/updated/deleted

    case NotFound(_) =>              complete(StatusCodes.NotFound)
    case AlreadyExists(_, _) =>      complete(StatusCodes.Conflict)

    case _: Error =>                 complete(StatusCodes.InternalServerError)
    case _ =>                        complete(StatusCodes.InternalServerError)
  }
}


