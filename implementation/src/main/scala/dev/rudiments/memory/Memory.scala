package dev.rudiments.memory

import dev.rudiments.data.Batch._
import dev.rudiments.data.DataCommand
import dev.rudiments.domain.{Cache, ID}
import dev.rudiments.hardcore._

import scala.collection.mutable

class Memory extends Skill[Event] {

  val story: mutable.ArrayBuffer[(Command, Event)] = mutable.ArrayBuffer.empty
  val conclusion: mutable.Map[ID, Event] = mutable.Map.empty
  val state: Cache = new Cache

  override def isDefinedAt(cmd: Command): Boolean = f.isDefinedAt(cmd)

  override def apply(cmd: Command): Result[Event] = f(cmd)

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

  val f: Skill[Event] = {
    case cmd: DataCommand => state(cmd) match {
      case s@Success(evt) =>
        memorize(cmd, evt)
        s
      case f@Failure(evt: Event) =>
        memorize(cmd, evt)
        f
    }

    case _ => ???
  }
}