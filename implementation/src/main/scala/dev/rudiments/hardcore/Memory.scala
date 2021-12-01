package dev.rudiments.hardcore


import scala.collection.mutable

class Memory {
  val state: mutable.SeqMap[ID, Data] = mutable.SeqMap.empty

  def read(id: ID): Out = Memory.read.action(Read(id), this)

  private val action: In => Out = Memory.read.action
    .orElse(Memory.create.action)
    .orElse(Memory.update.action)
    .orElse(Memory.delete.action).apply(_, this)

  private val reaction: Out => Data = Memory.read.reaction
    .orElse(Memory.create.reaction)
    .orElse(Memory.update.reaction)
    .orElse(Memory.delete.reaction)
    .apply(_, this)

  def apply(in: In): Out = {
    action(in) match {
      case evt: Event =>
        commit(evt)
        evt
      case report: Report => report
      case error: Error => error
    }
  }

  def commit(out: Out): Data = reaction(out)
}

object Memory {
  val read: Skill[Memory] = Skill[Memory](
    action = {
      case (Read(id), ctx) => ctx.state.get(id) match {
        case Some(found) => Readen(id, found)
        case None => NotFound(id)
      }
    },
    reaction = { //TODO remove reaction if In is Query
      case (Readen(_, found), _) => found
      case (NotFound(id), _) => throw new IllegalArgumentException(s"Not found $id")
    }
  )

  val create: Skill[Memory] = Skill[Memory](
    action = {
      case (Create(id, data), ctx) => ctx.read(id) match {
        case Readen(i, found) => AlreadyExist(i, found)
        case NotFound(i) => Created(i, data)
      }
    },
    reaction = {
      case (Created(id, data), ctx) =>
        ctx.state.get(id) match {
          case None =>
            ctx.state += id -> data
            ctx.state(id)
          case Some(_) => throw new IllegalArgumentException(s"Already exist $id")
        }
    }
  )

  val update: Skill[Memory] = Skill[Memory](
    action = {
      case (Update(id, data), ctx) => ctx.read(id) match {
        case Readen(i, found) => Updated(i, found, data)
        case e: Error => e
      }
    },
    reaction = {
      case (Updated(id, oldData, newData), ctx) =>
        ctx.state.get(id) match {
          case Some(v) if v == oldData =>
            ctx.state += id -> newData
            ctx.state(id)
          case Some(_) => throw new IllegalArgumentException(s"Conflict data for update $id")
          case None => throw new IllegalArgumentException(s"Not found $id")
        }
    }
  )

  val delete: Skill[Memory] = new Skill[Memory](
    action = {
      case (Delete(id), ctx) => ctx.read(id) match {
        case Readen(i, found) => Deleted(i, found)
        case e: Error => e
      }
    },
    reaction = {
      case (Deleted(id, data), ctx) =>
        ctx.state.get(id) match {
          case Some(v) if v == data =>
            ctx.state -= id
            data
          case Some(_) => throw new IllegalArgumentException(s"Conflict data for delete $id") // or just delete?
          case None => throw new IllegalArgumentException(s"Not found $id")
        }

    }
  )
}
