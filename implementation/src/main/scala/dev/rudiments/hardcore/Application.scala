package dev.rudiments.hardcore

trait Port[C <: Command, E <: Event]
abstract class PortWithResource[C <: Command, E <: Event, Resource](val s: Resource => Skill[E]) extends Port[C, E] {}
abstract class PortWithoutDependency[C <: Command, E <: Event](val s: Skill[E]) extends Port[C, E] {}

trait Service[C <: Command, E <: Event] extends Skill[E] {}

trait Adapter[C <: Command, E <: Event] extends Skill[E] {}

trait Application {}
