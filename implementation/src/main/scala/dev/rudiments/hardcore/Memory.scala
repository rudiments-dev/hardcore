package dev.rudiments.hardcore

import scala.collection.mutable

class Memory() extends Agent(Type.build[In], Type.build[Out]) {
  val state: mutable.SeqMap[ID, Thing] = mutable.SeqMap.empty

  def read(id: ID): Out = read.act(Read(id))

  val read: RO = RO {
    case Read(id) => state.get(id) match {
      case Some(found) => Readen(id, found)
      case None => NotFound(id)
    }
  }

  val create: RW = Skill(
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

  val update: RW = Skill(
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

  val delete: RW = Skill(
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

  val find: RO = RO {
    case Find(All) => Found(All, state.toMap) //TODO filter predicate
  }

  override val skill: RW = Skill(create, read, update, delete, find, commit)
}
