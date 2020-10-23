package dev.rudiments.hardcore.http

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive1, Route, StandardRoute}
import dev.rudiments.hardcore.{Ask, Command, Event, Message, PortWithResource, PortWithoutDependency, Reply, Skill}
import io.circe.Decoder
import dev.rudiments.hardcore.http.CirceSupport._

object HttpPorts {

  object DependencyLess {

    case class ActionPort[C <: Ask, E <: Reply]
    (
      command: C,
      override val s: Skill,
      result: Message => StandardRoute
    ) extends PortWithoutDependency(s) with Router {

      override val routes: Route = pathEndOrSingleSlash {
        result(s(command))
      }
    }

    case class EntityActionPort[C <: Ask, T: Decoder, E <: Reply]
    (
      command: T => C,
      override val s: Skill,
      result: Message => StandardRoute
    ) extends PortWithoutDependency(s) with Router {

      override val routes: Route = pathEndOrSingleSlash {
        entity(as[T]) { value =>
          result(s(command(value)))
        }
      }
    }

    case class GetDirectivePort[T, C <: Ask, E <: Reply]
    (
      directive: Directive1[T],
      command: T => C,
      override val s: Skill,
      result: Message => StandardRoute
    ) extends PortWithoutDependency(s) with Router {

      override val routes: Route = get {
        directive { t =>
          ActionPort(command(t), s, result).routes
        }
      }
    }

    case class GetPort[C <: Ask, E <: Reply]
    (
      command: C,
      override val s: Skill,
      result: Message => StandardRoute
    ) extends PortWithoutDependency(s) with Router {

      override val routes: Route = get {
        ActionPort(command, s, result).routes
      }
    }

    case class PostPort[C <: Ask, T: Decoder, E <: Reply]
    (
      command: T => C,
      override val s: Skill,
      result: Message => StandardRoute
    ) extends PortWithoutDependency(s) with Router {

      override val routes: Route = post {
        EntityActionPort(command, s, result).routes
      }
    }

    case class EmptyPostPort[C <: Ask, E <: Reply]
    (
      command: C,
      override val s: Skill,
      result: Message => StandardRoute
    ) extends PortWithoutDependency(s) with Router {

      override val routes: Route = post {
        ActionPort(command, s, result).routes
      }
    }

    case class PutPort[C <: Ask, T: Decoder, E <: Reply]
    (
      command: T => C,
      override val s: Skill,
      result: Message => StandardRoute
    ) extends PortWithoutDependency(s) with Router {

      override val routes: Route = put {
        EntityActionPort(command, s, result).routes
      }
    }

    case class DeletePort[C <: Ask, E <: Reply]
    (
      command: C,
      override val s: Skill,
      result: Message => StandardRoute
    ) extends PortWithoutDependency(s) with Router {

      override val routes: Route = delete {
        ActionPort(command, s, result).routes
      }
    }

  }

  object WithDependencies {
    case class ActionPort[C <: Ask, E <: Reply, Resource]
    (
      command: C,
      override val s: Resource => Skill,
      override val acquireResource: () => Resource,
      override val close: Resource => Unit,
      result: Message => StandardRoute
    ) extends PortWithResource(s, acquireResource, close) with Router {

      override val routes: Route = pathEndOrSingleSlash {
        result(safeExecute(command))
      }
    }

    case class EntityActionPort[C <: Ask, T: Decoder, E <: Reply, Resource]
    (
      command: T => C,
      override val s: Resource => Skill,
      override val acquireResource: () => Resource,
      override val close: Resource => Unit,
      result: Message => StandardRoute
    ) extends PortWithResource(s, acquireResource, close) with Router {

      override val routes: Route = pathEndOrSingleSlash {
        entity(as[T]) { value =>
          result(safeExecute(command(value)))
        }
      }
    }

    case class GetDirectivePort[T, C <: Ask, E <: Reply, Resource]
    (
      directive: Directive1[T],
      command: T => C,
      override val s: Resource => Skill,
      override val acquireResource: () => Resource,
      override val close: Resource => Unit,
      result: Message => StandardRoute
    ) extends PortWithResource(s, acquireResource, close) with Router {

      override val routes: Route = get {
        directive { t =>
          ActionPort(command(t), s, acquireResource, close, result).routes
        }
      }
    }

    case class GetPort[C <: Ask, E <: Reply, Resource]
    (
      command: C,
      override val s: Resource => Skill,
      override val acquireResource: () => Resource,
      override val close: Resource => Unit,
      result: Message => StandardRoute
    ) extends PortWithResource(s, acquireResource, close) with Router {

      override val routes: Route = get {
        ActionPort(command, s, acquireResource, close, result).routes
      }
    }

    case class PostPort[C <: Ask, T: Decoder, E <: Reply, Resource]
    (
      command: T => C,
      override val s: Resource => Skill,
      override val acquireResource: () => Resource,
      override val close: Resource => Unit,
      result: Message => StandardRoute
    ) extends PortWithResource(s, acquireResource, close) with Router {

      override val routes: Route = post {
        EntityActionPort(command, s, acquireResource, close, result).routes
      }
    }

    case class EmptyPostPort[C <: Ask, E <: Reply, Resource]
    (
      command: C,
      override val s: Resource => Skill,
      override val acquireResource: () => Resource,
      override val close: Resource => Unit,
      result: Message => StandardRoute
    ) extends PortWithResource(s, acquireResource, close) with Router {

      override val routes: Route = post {
        ActionPort(command, s, acquireResource, close, result).routes
      }
    }

    case class PutPort[C <: Ask, T: Decoder, E <: Reply, Resource]
    (
      command: T => C,
      override val s: Resource => Skill,
      override val acquireResource: () => Resource,
      override val close: Resource => Unit,
      result: Message => StandardRoute
    ) extends PortWithResource(s, acquireResource, close) with Router {

      override val routes: Route = put {
        EntityActionPort(command, s, acquireResource, close, result).routes
      }
    }

    case class DeletePort[C <: Ask, E <: Reply, Resource]
    (
      command: C,
      override val s: Resource => Skill,
      override val acquireResource: () => Resource,
      override val close: Resource => Unit,
      result: Message => StandardRoute
    ) extends PortWithResource(s, acquireResource, close) with Router {

      override val routes: Route = delete {
        ActionPort(command, s, acquireResource, close, result).routes
      }
    }
  }
}