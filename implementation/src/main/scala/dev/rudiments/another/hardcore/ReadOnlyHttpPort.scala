package dev.rudiments.another.hardcore

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import dev.rudiments.another._
import dev.rudiments.hardcore.http.{CompositeRouter, PrefixRouter, Router}
import io.circe.Encoder

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

class ReadOnlyHttpPort[A: Encoder : ClassTag, I <: In, T <: Tx, O <: Out, F: TypeTag](
  prefix: String,
  s: Service[In, In, T, O, O],
  customRoutes: Seq[(String, Router)] = Seq.empty,
  customIdRoutes: Seq[(String, ID[A] => Router)] = Seq.empty
) extends Port[I, T, O] with Router with FailFastCirceSupport {

  override val routes: Route = PrefixRouter(prefix,
    CompositeRouter(
      GetDirectivePort(PredicateDirective.apply, (d: Predicate) => FindAll.apply(d), s, responseWith),
      CompositeRouter(customRoutes.map { case (p, r) => PrefixRouter(p, r) } : _*)
    ),
    IDRouter(
      IDPath[A, F],
      { id: ID[A] => GetPort(Find(id), s, responseWith) },
      { id: ID[A] => CompositeRouter(customIdRoutes.map { case (p, r) => PrefixRouter(p, r(id)) } : _*) }
    )
  ).routes

  import dev.rudiments.hardcore.http.CirceSupport._
  def responseWith(out: Out): StandardRoute = out match {
    case Found(_, value: A) =>                complete(StatusCodes.OK, value)
    case FoundAll(content: Map[Identifier, A]) =>  complete(StatusCodes.OK, content.values.asInstanceOf[Iterable[A]])

    case NotFound(_) =>               complete(StatusCodes.NotFound)
    case AlreadyExists(_, _) =>       complete(StatusCodes.Conflict)
    case _: Error =>                  complete(StatusCodes.InternalServerError)
    case _ =>                         complete(StatusCodes.InternalServerError)
  }
}


