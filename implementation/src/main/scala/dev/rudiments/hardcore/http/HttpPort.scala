package dev.rudiments.hardcore.http

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive1, Route, StandardRoute}
import dev.rudiments.hardcore.{Command, Event, HardPort, HardSkill}
import io.circe.Decoder

import dev.rudiments.hardcore.http.CirceSupport._

case class ActionPort[C <: Command, E <: Event](
  command: C,
  override val h: HardSkill[C, E],
  result: E => StandardRoute
) extends HardPort(h) with Router {

  override val routes: Route = pathEndOrSingleSlash {
    result(h(command))
  }
}

case class EntityActionPort[C <: Command, T : Decoder, E <: Event](
  command: T => C,
  override val h: HardSkill[C, E],
  result: E => StandardRoute
) extends HardPort(h) with Router {

  override val routes: Route = pathEndOrSingleSlash {
    entity(as[T]) { value =>
      result(h(command(value)))
    }
  }
}

case class GetDirectivePort[T, C <: Command, E <: Event](
  directive: Directive1[T],
  command: T => C,
  override val h: HardSkill[C, E],
  result: E => StandardRoute) extends HardPort(h) with Router {

  override val routes: Route = get {
    directive { t =>
      ActionPort(command(t), h, result).routes
    }
  }
}

case class GetPort[C <: Command, E <: Event](
  command: C,
  override val h: HardSkill[C, E],
  result: E => StandardRoute
) extends HardPort(h) with Router {

  override val routes: Route = get {
    ActionPort(command, h, result).routes
  }
}

case class PostPort[C <: Command, T : Decoder, E <: Event](
  command: T => C,
  override val h: HardSkill[C, E],
  result: E => StandardRoute
) extends HardPort(h) with Router {

  override val routes: Route = post {
    EntityActionPort(command, h, result).routes
  }
}

case class EmptyPostPort[C <: Command, E <: Event](
  command: C,
  override val h: HardSkill[C, E],
  result: E => StandardRoute
) extends HardPort(h) with Router {

  override val routes: Route = post {
    ActionPort(command, h, result).routes
  }
}

case class PutPort[C <: Command, T : Decoder, E <: Event](
  command: T => C,
  override val h: HardSkill[C, E],
  result: E => StandardRoute
) extends HardPort(h) with Router {

  override val routes: Route = put {
    EntityActionPort(command, h, result).routes
  }
}

case class DeletePort[C <: Command, E <: Event](
  command: C,
  override val h: HardSkill[C, E],
  result: E => StandardRoute
) extends HardPort(h) with Router {

  override val routes: Route = delete {
    ActionPort(command, h, result).routes
  }
}