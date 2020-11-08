package dev.rudiments.memory

import dev.rudiments.data._
import dev.rudiments.domain.ID
import dev.rudiments.hardcore._

import scala.collection.mutable

class Memory(val of: Skill) extends Skill {

  val story: mutable.ArrayBuffer[(Ask, DataEvent)] = mutable.ArrayBuffer.empty
  val conclusion: mutable.Map[ID, DataEvent] = mutable.Map.empty

  override def isDefinedAt(cmd: Ask): Boolean = f.isDefinedAt(cmd)

  override def apply(cmd: Ask): Reply = f(cmd)

  def memorize(cmd: Ask, evt: DataEvent): DataEvent = evt match {
    case DataEvent(id) =>
      conclusion.get(id) match {
        case Some(existing) if existing == evt =>
          existing
        case _ =>
          conclusion += id -> evt
          story += cmd -> evt
          conclusion(id)
      }
    case Moved(oldKey, _, newKey, _) =>
      conclusion.get(oldKey) match {
        case Some(existing) if existing == evt =>
          existing
        case _ =>
          conclusion += oldKey -> evt
          conclusion += newKey -> evt
          story += cmd -> evt
          conclusion(newKey)
      }

    case c: Commit =>
      c.state.foreach { case(_, v) => memorize(cmd, v) }
      c

    case other => other
  }

  val f: Skill = {
    case cmd: Data with Command => of(cmd) match {
      case evt: DataEvent => memorize(cmd, evt)
      case other => other
    }

    case d: Data => of(d)

    case _ => ???
  }
}