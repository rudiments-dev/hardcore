package dev.rudiments.hardcore

import dev.rudiments.hardcore.http.HttpPorts

trait Port[C <: Command, E <: Event]
abstract class PortWithResource[C <: Command, E <: Event, Resource]
(
  val s: Resource => Skill,
  val acquireResource: () => Resource,
  val close: Resource => Unit
) extends Port[C, E] {
  def safeExecute(command: Command): Message = {
    val resource = acquireResource()
    val result = s(resource)(command)
    close(resource)
    result
  }
}
abstract class PortWithoutDependency[C <: Command, E <: Event](val s: Skill) extends Port[C, E] {
  val ports: HttpPorts.DependencyLess.type = dev.rudiments.hardcore.http.HttpPorts.DependencyLess
}

trait Service[C <: Command, E <: Event] extends Skill {}

trait Adapter[C <: Command, E <: Event] extends Skill {}

trait Application {}
