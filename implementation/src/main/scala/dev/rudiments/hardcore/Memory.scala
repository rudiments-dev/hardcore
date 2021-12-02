package dev.rudiments.hardcore


import scala.collection.mutable

class Memory {
  val state: mutable.SeqMap[ID, Data] = mutable.SeqMap.empty

  def apply(in: In): Out = {
    skill.act(in) match {
      case evt: Event =>
        skill.commit(evt)
        evt
      case other => other
    }
  }

  def read(id: ID): Out = read.act(Read(id))

  private val read: Skill = Skill(
    act = {
      case Read(id) => state.get(id) match {
        case Some(found) => Readen(id, found)
        case None => NotFound(id)
      }
    },
    commit = { //TODO remove reaction if In is Query?
      case Readen(_, found) => found
    }
  )

  private val create: Skill = Skill(
    act = {
      case Create(id, data) => read(id) match {
        case Readen(i, found) => AlreadyExist(i, found)
        case NotFound(i) => Created(i, data)
      }
    },
    commit = {
      case Created(id, data) =>
        state.get(id) match {
          case None =>
            state += id -> data
            state(id)
          case Some(_) => throw new IllegalArgumentException(s"Already exist $id")
        }
    }
  )

  private val update: Skill = Skill(
    act = {
      case Update(id, data) => read(id) match {
        case Readen(i, found) => Updated(i, found, data)
        case e: Error => e
      }
    },
    commit = {
      case Updated(id, oldData, newData) =>
        state.get(id) match {
          case Some(v) if v == oldData =>
            state += id -> newData
            state(id)
          case Some(_) => throw new IllegalArgumentException(s"Conflict data for update $id")
          case None => throw new IllegalArgumentException(s"Not found $id")
        }
    }
  )

  private val delete: Skill = Skill(
    act = {
      case Delete(id) => read(id) match {
        case Readen(i, found) => Deleted(i, found)
        case e: Error => e
      }
    },
    commit = {
      case Deleted(id, data) =>
        state.get(id) match {
          case Some(v) if v == data =>
            state -= id
            data
          case Some(_) => throw new IllegalArgumentException(s"Conflict data for delete $id") // or just delete?
          case None => throw new IllegalArgumentException(s"Not found $id")
        }

    }
  )

  private val skill: Skill = Skill(
    act = read.act.orElse(create.act).orElse(update.act).orElse(delete.act),
    commit = create.commit.orElse(update.commit).orElse(delete.commit)
  )
}
