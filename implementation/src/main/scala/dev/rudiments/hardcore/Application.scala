package dev.rudiments.hardcore

abstract class Port(val s: Skill) {}

abstract class HardPort[C <: Command, E <: Event](val h: HardSkill[C, E]) extends Port(asSkill(h)) {}

trait Service[C <: Command, E <: Event] extends HardSkill[C, E] {}

trait Adapter[C <: Command, E <: Event] extends HardSkill[C, E] {}

trait Pipe[C <: Command] {}
trait Pipeline[C <: Command] extends Pipe[C] {}

trait Drain[E <: Event] {}
trait Drainage[E <: Event] extends Drain[E] {}

trait Application {}
