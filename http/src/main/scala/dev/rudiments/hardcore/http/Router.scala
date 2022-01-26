package dev.rudiments.hardcore.http

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive0, Directive1, Route}
import dev.rudiments.hardcore.{Agent, All, ID, NoSkill, Path, Plain, Predicate, RW, ScalaTypes}

import java.sql.Date

abstract class Router extends Agent(All, All) {
  override val skill: RW = NoSkill
  def routes: Route
}

object PathOps {
  def pathDirective(path: Path): Directive0 = path.ids.map {
    case ID(None) => pathSingleSlash
    case ID(a) => pathPrefix(a.toString)
  }.reduce(_ and _)

  def seal(path: Path, routes: Route): Route = Route.seal(pathDirective(path) { routes })

  def plainId(p: Predicate): Directive1[ID] = p match {
    case ScalaTypes.ScalaLong =>    pathPrefix(LongNumber).map(l => ID(l))
    case ScalaTypes.ScalaInt =>     pathPrefix(IntNumber) .map(i => ID(i))
    case ScalaTypes.ScalaString =>  pathPrefix(Segment)   .map(i => ID(i))
    case Plain.UUID =>              pathPrefix(JavaUUID)  .map(uuid => ID(uuid))
    case Plain.Date =>              pathPrefix(Segment)   .map(s => ID(Seq(Date.valueOf(s))))
  }
}