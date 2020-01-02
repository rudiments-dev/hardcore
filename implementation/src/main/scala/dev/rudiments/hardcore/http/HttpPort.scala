package dev.rudiments.hardcore.http

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import dev.rudiments.hardcore.{Command, Event, Port, Skill}
import io.circe.Decoder

import dev.rudiments.hardcore.http.CirceSupport._

case class ActionPort[C <: Command, E <: Event](
  command: C,
  override val f: Skill[C, E],
  result: E => StandardRoute
) extends Port[C, E] with Router {

  override val routes: Route = pathEndOrSingleSlash {
    result(f(command))
  }
}

case class EntityActionPort[C <: Command, T : Decoder, E <: Event](
  command: T => C,
  override val f: Skill[C, E],
  result: E => StandardRoute
) extends Port[C, E] with Router {

  override val routes: Route = pathEndOrSingleSlash {
    entity(as[T]) { value =>
      result(f(command(value)))
    }
  }
}

case class GetPort[C <: Command, E <: Event](
  command: C,
  override val f: Skill[C, E],
  result: E => StandardRoute
) extends Port[C, E] with Router {

  override val routes: Route = get {
    ActionPort(command, f, result).routes
  }
}

case class PostPort[C <: Command, T : Decoder, E <: Event](
  command: T => C,
  override val f: Skill[C, E],
  result: E => StandardRoute
) extends Port[C, E] with Router {

  override val routes: Route = post {
    EntityActionPort(command, f, result).routes
  }
}

case class PutPort[C <: Command, T : Decoder, E <: Event](
  command: T => C,
  override val f: Skill[C, E],
  result: E => StandardRoute
) extends Port[C, E] with Router {

  override val routes: Route = put {
    EntityActionPort(command, f, result).routes
  }
}

case class DeletePort[C <: Command, E <: Event](
  command: C,
  override val f: Skill[C, E],
  result: E => StandardRoute
) extends Port[C, E] with Router {

  override val routes: Route = delete {
    ActionPort(command, f, result).routes
  }
}