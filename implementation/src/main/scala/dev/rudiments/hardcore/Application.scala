package dev.rudiments.hardcore

trait Port[C <: Command, E <: Event] {
  val f: Skill[C, E]
}

trait Service[C <: Command, E <: Event] extends Skill[C, E] {}

trait Adapter[C <: Command, E <: Event] extends Skill[C, E] {}

trait Pipe[C <: Command] {}
trait Pipeline[C <: Command] extends Pipe[C] {}

trait Drain[E <: Event] {}
trait Drainage[E <: Event] extends Drain[E] {}

trait Application {}
