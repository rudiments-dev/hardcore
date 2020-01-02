package dev.rudiments.hardcore.http

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import dev.rudiments.hardcore.{Command, Event, Port, Skill}
import io.circe.Decoder
import dev.rudiments.hardcore.http.CirceSupport._
import dev.rudiments.hardcore.types.{DTO, HardType, ID}

import scala.reflect.runtime.universe.TypeTag
case class ActionIDPort[C <: Command, T <: DTO : HardType : Decoder, K : TypeTag, E <: Event](
  command: ID[T] => C,
  override val f: Skill[C, E],
  result: E => StandardRoute
) extends Port[C, E] with Router {

  override val routes: Route = pathEndOrSingleSlash {
    IDPath[T, K](implicitly[HardType[T]], implicitly[TypeTag[K]]) { id: ID[T] =>
      result(f(command(id)))
    }
  }
}

case class EntityActionIDPort[C <: Command, T <: DTO : HardType : Decoder, K : TypeTag, E <: Event](
  command: (T, ID[T]) => C,
  override val f: Skill[C, E],
  result: E => StandardRoute
) extends Port[C, E] with Router {

  override val routes: Route = pathEndOrSingleSlash {
    IDPath[T, K](implicitly[HardType[T]], implicitly[TypeTag[K]]) { id: ID[T] =>
      entity(as[T]) { value =>
        result(f(command(value, id)))
      }
    }
  }
}

case class GetIDPort[C <: Command, T <: DTO : HardType : Decoder, K : TypeTag,  E <: Event](
  command: ID[T] => C,
  override val f: Skill[C, E],
  result: E => StandardRoute
) extends Port[C, E] with Router {

  override val routes: Route = get {
    ActionIDPort(command, f, result).routes
  }
}

case class PostIDPort[C <: Command, T <: DTO : HardType : Decoder, K : TypeTag,  E <: Event](
  command: (T, ID[T]) => C,
  override val f: Skill[C, E],
  result: E => StandardRoute
) extends Port[C, E] with Router {

  override val routes: Route = post {
    EntityActionIDPort(command, f, result).routes
  }
}

case class PutIDPort[C <: Command, T <: DTO : HardType : Decoder, K : TypeTag,  E <: Event](
  command: (T, ID[T]) => C,
  override val f: Skill[C, E],
  result: E => StandardRoute
) extends Port[C, E] with Router {

  override val routes: Route = put {
    EntityActionIDPort(command, f, result).routes
  }
}

case class DeleteIDPort[C <: Command, T <: DTO : HardType : Decoder, K : TypeTag,  E <: Event](
  command: ID[T] => C,
  override val f: Skill[C, E],
  result: E => StandardRoute
) extends Port[C, E] with Router {

  override val routes: Route = delete {
    ActionIDPort(command, f, result).routes
  }
}