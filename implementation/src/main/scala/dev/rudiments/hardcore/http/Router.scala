package dev.rudiments.hardcore.http

import java.sql.Date

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive1, Route}
import dev.rudiments.types.hard.ScalaTypes
import dev.rudiments.types.{ID, Plain, Thing, Type}

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

case class IDRouter(idDirective: Directive1[ID], idRouters: (ID => Router)*) extends Router {
  override val routes: Route = idDirective { id =>
    CompositeRouter(idRouters.map(t => t(id)): _*).routes
  }
}

case class ResourceRouter(
  prefix: String,
  idField: String,
  pathRouters: Seq[Router] = Seq.empty,
  idRouters: Seq[ID => Router] = Seq.empty
)(implicit t: Type) extends Router {
  override val routes: Route = PrefixRouter(prefix,
    IDRouter(IDPath(t.fields(idField).`type`)(t), idRouters: _*),
    CompositeRouter(pathRouters: _*)
  ).routes
}

object IDPath {
  def apply(f: Thing)(implicit t: Type): Directive1[ID] = f match {
    case ScalaTypes.ScalaLong =>  pathPrefix(LongNumber).map(l => ID(l))
    case ScalaTypes.ScalaInt =>   pathPrefix(IntNumber).map(l => ID(l))
    case Plain.Text(_) =>         pathPrefix(Segment).map(s => ID(s))
    case Plain.UUID =>            pathPrefix(JavaUUID).map(u => ID(u))
    case Plain.Date =>            pathPrefix(Segment).map(s => ID(Date.valueOf(s)))
    case other => ???
  }
}
