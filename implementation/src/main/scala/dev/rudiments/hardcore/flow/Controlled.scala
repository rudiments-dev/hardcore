package dev.rudiments.hardcore.flow

import dev.rudiments.hardcore.Error.NoHandler
import dev.rudiments.hardcore.{Command, Event, HardSkill, Skill}

class Controlled(skill: Skill)(implicit flow: ControlFlow) extends Skill {

  override def isDefinedAt(cmd: Command): Boolean = cmd match {
    case c: Command => skill.isDefinedAt(c)
    case c: ControlCommand => false // for future use
    case other => false
  }

  override def apply(cmd: Command): Event = flow.lastMessage(cmd) match {
    case Some(command: Command) => this(command)
    case Some(evt: Event) => evt
    case None => cmd match {
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
    call match {
      case c: Command if skill.isDefinedAt(c) =>
        flow.put(call, InProgress)
        val result = skill(c)
        flow.put(call, result)
        result
      case other =>
        flow.put(other, NoHandler)
        NoHandler
    }
  }
}
