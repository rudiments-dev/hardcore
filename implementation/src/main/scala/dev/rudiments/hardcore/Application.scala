package dev.rudiments.hardcore

abstract class Port[C <: Command, E <: Event](val s: Skill[E]) {}

trait Service[C <: Command, E <: Event] extends Skill[E] {}

trait Adapter[C <: Command, E <: Event] extends Skill[E] {}

trait Application {}
