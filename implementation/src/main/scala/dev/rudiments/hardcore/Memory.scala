package dev.rudiments.hardcore

import scala.collection.mutable

class Memory(val idIs: Predicate, val dataIs: Predicate) extends Agent(idIs, dataIs) {
  val state: mutable.SeqMap[ID, Thing] = mutable.SeqMap.empty

  def read(id: ID): Out = Memory.read(this).act(Read(id))

  override val skill: RW = Skill(
    Memory.create(this),
    Memory.read(this),
    Memory.update(this),
    Memory.delete(this),
    Memory.find(this),
    Memory.commit(this)
  )
}

object Memory {
  def read(implicit ctx: Memory): RO = RO {
    case Read(id) => ctx.state.get(id) match {
      case Some(found) => Readen(id, found)
      case None => NotFound(id)
    }
  }

  def create(implicit ctx: Memory): RW = RW (
    act = {
      case Create(id, data) => ctx.read(id) match {
        case Readen(i, found) => AlreadyExist(i, found)
        case NotFound(i) => Created(i, data)
      }
    },
    commit = {
      case Created(id, data) =>
        ctx.state.get(id) match {
          case None =>
            ctx.state += id -> data
            ctx.state(id)
          case Some(_) => throw new IllegalArgumentException(s"Already exist $id")
        }
    }
  )

  def update(implicit ctx: Memory): RW = RW (
    act = {
      case Update(id, data) => ctx.read(id) match {
        case Readen(i, found) => Updated(i, found, data)
        case e: Error => e
      }
    },
    commit = {
      case Updated(id, oldData, newData) =>
        ctx.state.get(id) match {
          case Some(v) if v == oldData =>
            ctx.state += id -> newData
            ctx.state(id)
          case Some(_) => throw new IllegalArgumentException(s"Conflict data for update $id")
          case None => throw new IllegalArgumentException(s"Not found $id")
        }
    }
  )

  def delete(implicit ctx: Memory): RW = RW (
    act = {
      case Delete(id) => ctx.read(id) match {
        case Readen(i, found) => Deleted(i, found)
        case e: Error => e
      }
    },
    commit = {
      case Deleted(id, data) =>
        ctx.state.get(id) match {
          case Some(v) if v == data =>
            ctx.state -= id
            data
          case Some(_) => throw new IllegalArgumentException(s"Conflict data for delete $id") // or just delete?
          case None => throw new IllegalArgumentException(s"Not found $id")
        }

    }
  )

  def find(implicit ctx: Memory): RO = RO {
    case Find(All) => Found(All, ctx.state.toMap) //TODO filter predicate
      //TODO compare Agents?
  }

  def commit(implicit ctx: Memory): RW = RW (
    act = {
      case Apply(commands) =>
        val result: Seq[(In, Out)] = Apply.collapse(commands).values.map { cmd =>
          cmd -> ctx.skill.act(cmd)
        }.toSeq
        Commit(result)
    },
    commit = {
      case Commit(delta, extra) =>
        val data = delta.map {
          case (id, evt) => id -> ctx.skill.commit(evt)
        }
        extra.foreach {
          case (_, evt: Event) => ctx.skill.commit(evt) //TODO log? ignore?
        }
        new Data(Index(All, All), data)
    }
  )
}