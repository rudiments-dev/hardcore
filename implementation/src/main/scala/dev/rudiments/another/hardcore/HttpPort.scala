package dev.rudiments.another.hardcore

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive1, Route, StandardRoute}
import dev.rudiments.another.{In, Out, Tx}
import dev.rudiments.hardcore.http.{CompositeRouter, PrefixRouter, Router}
import io.circe.Decoder
import dev.rudiments.hardcore.http.CirceSupport._

import java.sql.Date
import java.util.UUID
trait Port[Rq <: In, T <: Tx, Rs <: Out]

case class ActionPort[I <: In, T <: Tx, O <: Out](
  in: I,
  s: Service[I, I, T, O, O],
  result: O => StandardRoute
) extends Port[I, T, O] with Router {

  override val routes: Route = pathEndOrSingleSlash {
    result(s(in))
  }
}

case class EntityActionPort[I <: In, B: Decoder, T <: Tx, O <: Out](
  in: B => I,
  s: Service[I, I, T, O, O],
  result: O => StandardRoute
) extends Port[I, T, O] with Router {

  override val routes: Route = pathEndOrSingleSlash {
    entity(as[B]) { value =>
      result(s(in(value)))
    }
  }
}

case class GetDirectivePort[D, I <: In, T <: Tx, O <: Out](
  directive: Directive1[D],
  in: D => I,
  s: Service[I, I, T, O, O],
  result: O => StandardRoute
) extends Port[I, T, O] with Router {

  override val routes: Route = get {
    directive { t =>
      ActionPort(in(t), s, result).routes
    }
  }
}

case class GetPort[I <: In, T <: Tx, O <: Out](
  in: I,
  s: Service[I, I, T, O, O],
  result: O => StandardRoute
) extends Port[I, T, O] with Router {

  override val routes: Route = get {
    ActionPort(in, s, result).routes
  }
}

case class PostPort[I <: In, B: Decoder, T <: Tx, O <: Out](
  in: B => I,
  s: Service[I, I, T, O, O],
  result: O => StandardRoute
) extends Port[I, T, O] with Router {

  override val routes: Route = post {
    EntityActionPort(in, s, result).routes
  }
}

case class EmptyPostPort[I <: In, T <: Tx, O <: Out](
  in: I,
  s: Service[I, I, T, O, O],
  result: O => StandardRoute
) extends Port[I, T, O] with Router {

  override val routes: Route = post {
    ActionPort(in, s, result).routes
  }
}

case class PutPort[I <: In, B : Decoder, T <: Tx, O <: Out](
  in: B => I,
  s: Service[I, I, T, O, O],
  result: O => StandardRoute
) extends Port[I, T, O] with Router {

  override val routes: Route = put {
    EntityActionPort(in, s, result).routes
  }
}

case class DeletePort[I <: In, T <: Tx, O <: Out](
  in: I,
  s: Service[I, I, T, O, O],
  result: O => StandardRoute
) extends Port[I, T, O] with Router {

  override val routes: Route = delete {
    ActionPort(in, s, result).routes
  }
}

case class DeleteDirectivePort[D, I <: In, T <: Tx, O <: Out](
  directive: Directive1[D],
  in: D => I,
  s: Service[I, I, T, O, O],
  result: O => StandardRoute
) extends Port[I, T, O] with Router {

  override val routes: Route = delete {
    directive { t =>
      ActionPort(in(t), s, result).routes
    }
  }
}

case class IDRouter[A](idDirective: Directive1[ID[A]], idRouters: (ID[A] => Router)*) extends Router {
  override val routes: Route = idDirective { id =>
    CompositeRouter(idRouters.map(t => t(id)): _*).routes
  }
}

import scala.reflect.runtime.universe.{TypeTag, typeOf}
case class ResourceRouter[A, F : TypeTag](
  prefix: String,
  id: A => ID[A],
  pathRouters: Seq[Router] = Seq.empty,
  idRouters: Seq[ID[A] => Router] = Seq.empty
) extends Router {
  override val routes: Route = PrefixRouter(prefix,
    IDRouter(IDPath.apply[A, F], idRouters: _*),
    CompositeRouter(pathRouters: _*)
  ).routes
}

object IDPath {
  def apply[A, F: TypeTag]: Directive1[ID[A]] = {
    if(typeOf[F] =:= typeOf[Long])        pathPrefix(LongNumber).map(l => ID[A](Seq(l)))
    else if(typeOf[F] =:= typeOf[Int])    pathPrefix(IntNumber).map(i => ID[A](Seq(i)))
    else if(typeOf[F] =:= typeOf[String]) pathPrefix(Segment).map(s => ID[A](Seq(s)))
    else if(typeOf[F] =:= typeOf[UUID])   pathPrefix(JavaUUID).map(s => ID[A](Seq(s)))
    else if(typeOf[F] =:= typeOf[Date])   pathPrefix(Segment).map(s => ID[A](Seq(Date.valueOf(s))))
    else ??? //TODO enums
  }
}

object PredicateDirective {
  def apply: Directive1[Predicate] = parameterMultiMap.map {
    params =>
      if(params.isEmpty) {
        All.asInstanceOf[Predicate]
      } else {
        ???
      }
  }
}