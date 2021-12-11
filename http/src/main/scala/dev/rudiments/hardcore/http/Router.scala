package dev.rudiments.hardcore.http

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive0, Directive1, Route}
import dev.rudiments.hardcore.{ID, Path, Plain, Predicate, ScalaTypes}

import java.sql.Date

trait Router {
  val routes: Route
  val path: Path

  lazy val pathDirective: Directive0 = path.ids.map {
    case ID(None) => pathSingleSlash
    case ID(a) => pathPrefix(a.toString)
  }.reduce(_ and _)

  def seal(): Route = Route.seal(pathDirective { routes })

  def plainId(p: Predicate): Directive1[ID] = p match {
    case ScalaTypes.ScalaLong =>    pathPrefix(LongNumber).map(l => ID(l))
    case ScalaTypes.ScalaInt =>     pathPrefix(IntNumber) .map(i => ID(i))
    case ScalaTypes.ScalaString =>  pathPrefix(Segment)   .map(i => ID(i))
    case Plain.UUID =>              pathPrefix(JavaUUID)  .map(uuid => ID(uuid))
    case Plain.Date =>              pathPrefix(Segment)   .map(s => ID(Seq(Date.valueOf(s))))
  }
}