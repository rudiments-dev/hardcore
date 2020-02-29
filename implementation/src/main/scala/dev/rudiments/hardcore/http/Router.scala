package dev.rudiments.hardcore.http

import java.sql.Date

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive1, Route}
import dev.rudiments.hardcore.types.ID

trait Router {
  val routes: Route
}
object Router {
  def apply(route: Route): Router = new Router {
    override val routes: Route = route
  }
}

case class CompositeRouter(routers: Router*) extends Router {
  override val routes: Route = routers.map(_.routes).foldRight(reject(): Route)(_ ~ _)
}

case class PrefixRouter(prefix: String, routers: Router*) extends Router {
  override val routes: Route =
    pathPrefix(prefix) {
      CompositeRouter(routers: _*).routes
    }
}

case class IDRouter[T](idDirective: Directive1[ID[T]], idRouters: (ID[T] => Router)*) extends Router {
  override val routes: Route = idDirective { id =>
    CompositeRouter(idRouters.map(t => t(id)): _*).routes
  }
}

import scala.reflect.runtime.universe.TypeTag
case class ResourceRouter[T, K : TypeTag](
  prefix: String,
  pathRouters: Seq[Router] = Seq.empty,
  idRouters: Seq[(ID[T] => Router)] = Seq.empty
) extends Router {
  override val routes: Route = PrefixRouter(prefix,
    IDRouter(IDPath[T, K], idRouters: _*),
    CompositeRouter(pathRouters: _*)
  ).routes
}

object IDPath {
  import scala.reflect.runtime.universe.{TypeTag, typeOf}
  def apply[A, K: TypeTag]: Directive1[ID[A]] = {
          if(typeOf[K] =:= typeOf[Long])   pathPrefix(LongNumber).map(l => ID[A, Long](l))
    else  if(typeOf[K] =:= typeOf[Int])    pathPrefix(IntNumber).map(i => ID[A, Int](i))
    else  if(typeOf[K] =:= typeOf[String]) pathPrefix(Segment).map(s => ID[A, String](s))
    else  if(typeOf[K] =:= typeOf[Date])   pathPrefix(Segment).map(s => ID[A, Date](Date.valueOf(s)))
    else ??? //TODO enums
  }
}
