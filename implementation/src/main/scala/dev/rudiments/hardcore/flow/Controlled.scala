package dev.rudiments.hardcore.flow

import dev.rudiments.hardcore.Error.NoHandler
import dev.rudiments.hardcore.{Command, Event, Message, Result, Skill, SkillResult}

class Controlled[E <: Event](skill: Skill[E])(implicit flow: ControlFlow) extends Skill[E] {

  override def isDefinedAt(cmd: Command): Boolean = skill.isDefinedAt(cmd)

  override def apply(cmd: Command): Result[E] = execute(cmd)

  private def execute(cmd: Command): Result[E] = {
    cmd match {
      case c: Command if skill.isDefinedAt(c) =>
        flow.put(cmd, InProgress)
        val result = skill(c)
        flow.put(cmd, result.asInstanceOf[SkillResult[Message, Message]].merge)
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
