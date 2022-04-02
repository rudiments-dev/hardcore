package dev.rudiments.hardcore

import scala.collection.mutable

class Memory(val idIs: Predicate, val dataIs: Predicate) extends AgentRead(idIs, dataIs) {
  val state: mutable.SeqMap[ID, Thing] = mutable.SeqMap.empty

  override def read(id: ID): Out = Memory.read(this).query(Read(id))

  override val skill: RW = Memory.skill(this)
}

object Memory {
  def skill(implicit ctx: Memory): RW = Skill(
    Memory.create(ctx),
    Memory.read(ctx),
    Memory.update(ctx),
    Memory.delete(ctx),
    Memory.find(ctx),
    Memory.commit(ctx)
  )

  def read(implicit ctx: Memory): RO = RO {
    case Read(id) => ctx.state.get(id) match {
      case Some(found) => Readen(id, found)
      case None => NotFound(id)
    }
  }

  def create(implicit ctx: Memory): RW = RW (
    query = {
      case Create(id, data) => ctx >> id match {
        case Readen(i, found) => AlreadyExist(i, found)
        case NotFound(i) => Created(i, data)
      }
    },
    write = {
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
    query = {
      case Update(id, data) => ctx >> id match {
        case Readen(i, found) => Updated(i, found, data)
        case e: Error => e
      }
    },
    write = {
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
    query = {
      case Delete(id) => ctx >> id match {
        case Readen(i, found) => Deleted(i, found)
        case e: Error => e
      }
    },
    write = {
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
    case Find(All) => Found(All, ctx.state.toMap)
    case Find(p) => Found(p, ctx.state.filter { case (_, v) => p.validate(v) }.toMap)
      //TODO compare Agents?
  }

  def commit(implicit ctx: Memory): RW = RW (
    query = {
      case Apply(commands) =>
        val result: Seq[(In, Out)] =
          Apply.collapse(commands).values
            .map { cmd => cmd -> (ctx <<? cmd) }.toSeq
        Commit(result)
    },
    write = {
      case Commit(delta, extra) =>
        val data = delta.map { case (id, evt) => id -> (ctx <<! evt) }
        extra.foreach {
          case (_, evt: Event) => ctx <<! evt //TODO log? ignore?
        }
        new Data(Index(All, All), data)
    }
  )
}