package dev.rudiments.hardcore.flow

import dev.rudiments.hardcore.Error.NoHandler
import dev.rudiments.hardcore.{Command, Event, Result, Skill}

class Controlled[E <: Event](skill: Skill[E])(implicit flow: ControlFlow) extends Skill[E] {

  override def isDefinedAt(cmd: Command): Boolean = cmd match {
    case c: Command if skill.isDefinedAt(c) => true
    case _: ControlCommand => false //for future use
    case other => false
  }

  override def apply(cmd: Command): Result[E] = cmd match {
    case c: AlwaysDo => execute(c)
    case c: BulkRead => execute(c)
    case c: SideEffect => execute(c) //TODO think!
    case c: CacheSingle =>
      val stateful = flow.state.get(c.key).map(s => s.last)
      stateful match {
        case Some((command, nested: Command)) if command == cmd => this(nested)
        case Some((command, event: Event)) if command == cmd => event.toEither
        case Some((command, other)) if command != cmd => execute(cmd)
        case None => execute(c)
      }
    case c: Command =>
      flow.lastMessage(c) match { //TODO wrong?
        case Some(command: Command) => this(command)
        case Some(evt: Event) => evt.toEither
        case None => execute(cmd)
      }
  }

  private def execute(cmd: Command): Result[E] = {
    cmd match {
      case c: Command if skill.isDefinedAt(c) =>
        flow.put(cmd, InProgress)
        val result = skill(c)
        flow.put(cmd, result.merge)
        result
      case other =>
        flow.put(other, NoHandler)
        NoHandler.toEither
    }
  }

  def internalCall(parent: Command, call: Command): Result[E] = {
    flow.put(parent, call)
    execute(call)
  }
}
