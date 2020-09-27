package dev.rudiments.memory

import dev.rudiments.data.Batch._
import dev.rudiments.domain.ID
import dev.rudiments.hardcore._

import scala.collection.mutable

class Memory[E <: Event](val of: Skill) extends Skill {

  val story: mutable.ArrayBuffer[(Command, Event)] = mutable.ArrayBuffer.empty
  val conclusion: mutable.Map[ID, Event] = mutable.Map.empty

  override def isDefinedAt(cmd: Command): Boolean = f.isDefinedAt(cmd)

  override def apply(cmd: Command): Message = f(cmd)

  def memorize(cmd: Command, evt: Event): Event = evt match {
    case o: One =>
      conclusion.get(o.id) match {
        case Some(existing) if existing == evt =>
          existing
        case _ =>
          conclusion += o.id -> evt
          story += cmd -> evt
          conclusion(o.id)
      }

    case c: Commit =>
      c.state.foreach { case(_, v) => memorize(cmd, v) }
      c

    case other => other
  }

  val f: Skill = {
    case cmd: Command => of(cmd) match {
      case evt: Event => memorize(cmd, evt)
      case other => other
    }

    case _ => ???
  }
}