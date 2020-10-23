package dev.rudiments.hardcore

import dev.rudiments.hardcore.http.HttpPorts

trait Port[C <: Ask, E <: Reply]
abstract class PortWithResource[C <: Ask, E <: Reply, Resource]
(
  val s: Resource => Skill,
  val acquireResource: () => Resource,
  val close: Resource => Unit
) extends Port[C, E] {
  def safeExecute(command: Ask): Reply = {
    val resource = acquireResource()
    val result = s(resource)(command)
    close(resource)
    result
  }
}
abstract class PortWithoutDependency[C <: Ask, E <: Reply](val s: Skill) extends Port[C, E] {
  val ports: HttpPorts.DependencyLess.type = dev.rudiments.hardcore.http.HttpPorts.DependencyLess
}

trait Service[C <: Ask, E <: Reply] extends Skill {}

trait Adapter[C <: Ask, E <: Reply] extends Skill {}

trait Application {}
