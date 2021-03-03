package dev.rudiments.another.hardcore

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive, Directive1, PathMatcher1, Route, StandardRoute}
import dev.rudiments.another.{In, Out, Tx}
import dev.rudiments.hardcore.http.{CompositeRouter, PrefixRouter, Router}
import io.circe.Decoder
import dev.rudiments.hardcore.http.CirceSupport._

import java.sql.Date
import java.util.UUID
import scala.language.postfixOps
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

case class PostDirectivePort[D, I <: In, T <: Tx, O <: Out](
  directive: Directive1[D],
  in: D => I,
  s: Service[I, I, T, O, O],
  result: O => StandardRoute
) extends Port[I, T, O] with Router {

  override val routes: Route = post {
    directive { t =>
      ActionPort(in(t), s, result).routes
    }
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
  def apply[A, F: TypeTag]: Directive1[ID[A]] = pathPrefix(prefix[F]).map(l => ID[A](Seq(l)))

  def prefix[F: TypeTag]: PathMatcher1[F] = {
    if(typeOf[F] =:= typeOf[Long])        LongNumber
    else if(typeOf[F] =:= typeOf[Int])    IntNumber
    else if(typeOf[F] =:= typeOf[String]) Segment
    else if(typeOf[F] =:= typeOf[UUID])   JavaUUID
    else if(typeOf[F] =:= typeOf[Date])   Segment.map(s => Date.valueOf(s))
    else ??? //TODO enums
  }.asInstanceOf[PathMatcher1[F]]

  def apply[A, F1 : TypeTag, F2 : TypeTag]: Directive1[ID[A]] = pathPrefix(prefix[F1] / prefix[F2]).tmap { k => ID[A](Seq(k._1, k._2)) }
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