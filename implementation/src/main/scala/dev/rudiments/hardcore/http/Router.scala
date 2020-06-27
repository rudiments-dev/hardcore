package dev.rudiments.hardcore.http

import java.sql.Date

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive1, Route}
import dev.rudiments.hardcore.types.{FieldType, ID, ScalaTypes, Type}

import scala.language.implicitConversions

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
    IDRouter(IDPath(t.fields(idField).kind)(t), idRouters: _*),
    CompositeRouter(pathRouters: _*)
  ).routes
}

import dev.rudiments.hardcore.types.SoftID.SoftID1
import dev.rudiments.hardcore.types.Types
object IDPath {
  implicit def toID(directive: Directive1[SoftID1]): Directive1[ID] = directive.map(_.asInstanceOf[ID])

  def apply(f: FieldType)(implicit t: Type): Directive1[ID] = f match {
    case ScalaTypes.ScalaLong =>  pathPrefix(LongNumber).map(l => SoftID1(l))
    case ScalaTypes.ScalaInt =>   pathPrefix(IntNumber).map(l => SoftID1(l))
    case Types.Text(_) =>         pathPrefix(Segment).map(s => SoftID1(s))
    case Types.UUID =>            pathPrefix(JavaUUID).map(u => SoftID1(u))
    case Types.Date =>            pathPrefix(Segment).map(s => SoftID1(Date.valueOf(s)))
    case other => ???
  }
}
