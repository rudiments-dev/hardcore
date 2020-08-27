package dev.rudiments.another

import java.sql.Date

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive1, Route}

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
)(implicit spec: Spec) extends Router {
  override val routes: Route = PrefixRouter(prefix,
    IDRouter(IDPath(spec.fields(idField).thing), idRouters: _*),
    CompositeRouter(pathRouters: _*)
  ).routes
}

object IDPath {
  def apply(f: Thing): Directive1[ID] = f match {
    case ScalaTypes.ScalaLong =>  pathPrefix(LongNumber).map(l => ID(Seq(l)))
    case ScalaTypes.ScalaInt =>   pathPrefix(IntNumber).map(l => ID(Seq(l)))
    case Plain.Text(_) =>         pathPrefix(Segment).map(s => ID(Seq(s)))
    case Plain.UUID =>            pathPrefix(JavaUUID).map(u => ID(Seq(u)))
    case Plain.Date =>            pathPrefix(Segment).map(s => ID(Seq(Date.valueOf(s))))
    case other => ???
  }
}
