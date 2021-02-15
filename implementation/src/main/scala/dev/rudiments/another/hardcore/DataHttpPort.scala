package dev.rudiments.another.hardcore

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.another._
import dev.rudiments.hardcore.http.{CompositeRouter, PrefixRouter, Router}
import io.circe.{Decoder, Encoder}

import scala.reflect.runtime.universe.TypeTag
class DataHttpPort[A : Encoder : Decoder, I <: In, T <: Tx, O <: Out, F : TypeTag](
  prefix: String,
  identify: A => ID[A],
  s: Service[In, In, T, O, O],
  customRoutes: Seq[(String, Router)] = Seq.empty,
  customIdRoutes: Seq[(String, ID[A] => Router)] = Seq.empty
) extends Port[I, T, O] with Router with FailFastCirceSupport {

  override val routes: Route = PrefixRouter(prefix,
    CompositeRouter(
      GetDirectivePort(PredicateDirective.apply, (d: Predicate) => FindAll.apply(d), s, responseWith),
      PostPort((value: A) => Create(identify(value), value), s, responseWith),
      PostPort((batch: Seq[A]) => CreateAll(batch.groupBy(identify).view.mapValues(_.head).toMap[ID[A], A]), s, responseWith),
      PutPort((batch: Seq[A]) => ReplaceAll(batch.groupBy(identify).view.mapValues(_.head).toMap[ID[A], A]), s, responseWith),
      DeleteDirectivePort(PredicateDirective.apply, DeleteUsing.apply, s, responseWith),
      CompositeRouter(customRoutes.map { case (p, r) => PrefixRouter(p, r) } : _*)
    ),
    IDRouter(
      IDPath[A, F],
      { id: ID[A] => GetPort(Find(id), s, responseWith) },
      { id: ID[A] => PutPort((value: A) => {
        val valueId = identify(value)
        if(id == valueId) {
          Update(id, value)
        } else {
          Move(id, valueId, value)
        }
      }, s, responseWith) },
      { id: ID[A] => DeletePort(Delete(id), s, responseWith) },
      { id: ID[A] => CompositeRouter(customIdRoutes.map { case (p, r) => PrefixRouter(p, r(id)) } : _*) }
    )
  ).routes

  import dev.rudiments.hardcore.http.CirceSupport._
  def responseWith(out: Out): StandardRoute = out match {
    case Created(_, value: A) =>              complete(StatusCodes.Created, value)
    case Found(_, value: A) =>                complete(StatusCodes.OK, value)
    case FoundAll(content: Map[ID[_], A]) =>  complete(StatusCodes.OK, content.values.asInstanceOf[Iterable[A]])
    case Updated(_, _, newValue: A) =>        complete(StatusCodes.OK, newValue)
    case Moved(_, _, _, newValue: A) =>       complete(StatusCodes.OK, newValue)
    case Deleted(_, _) =>                     complete(StatusCodes.NoContent)
    case Commit(_) =>                         complete(StatusCodes.OK) //TODO report amount of created/updated/deleted

    case NotFound(_) =>               complete(StatusCodes.NotFound)
    case AlreadyExists(_, _) =>       complete(StatusCodes.Conflict)
    case _: Error =>                  complete(StatusCodes.InternalServerError)
    case _ =>                         complete(StatusCodes.InternalServerError)
  }
}


