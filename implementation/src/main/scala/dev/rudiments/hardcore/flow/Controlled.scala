package dev.rudiments.hardcore.flow

import dev.rudiments.hardcore.Error.NoHandler
import dev.rudiments.hardcore.{Command, Event, Skill}

class Controlled(skill: Skill)(implicit flow: ControlFlow) extends Skill {

  override def isDefinedAt(cmd: Command): Boolean = cmd match {
    case c: Command if skill.isDefinedAt(c) => true
    case _: ControlCommand => false //for future use
    case other => false
  }

  override def apply(cmd: Command): Event = cmd match {
    case c: AlwaysDo => execute(c)
    case c: SideEffect => execute(c) //TODO think!
    case c: CacheSingle =>
      flow.cachedContext.get(c.key) match {
        case Some((command, nested: Command)) if command == cmd => this(nested)
        case Some((command, event: Event)) if command == cmd => event
        case Some((command, other)) if command != cmd => execute(cmd)
        case None => execute(c)
      }
    case c: Command =>
      flow.lastMessage(c) match { //TODO wrong?
        case Some(command: Command) => this(command)
        case Some(evt: Event) => evt
        case None => execute(cmd)
      }
  }

  private def execute(cmd: Command): Event = {
    cmd match {
      case c: Command if skill.isDefinedAt(c) =>
        flow.put(cmd, InProgress)
        val result = skill(c)
        flow.put(cmd, result)
        result
      case other =>
        flow.put(other, NoHandler)
        NoHandler
    }
  }

  def internalCall(parent: Command, call: Command): Event = {
    flow.put(parent, call)
    execute(call)
  }
}
