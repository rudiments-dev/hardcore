package dev.rudiments.hardcore

import scala.collection.mutable

class Memory(
  override val in: Predicate = Type.build[In],
  override val out: Predicate = Type.build[Out]
) extends Agent(in, out) {
  val state: mutable.SeqMap[ID, Thing] = mutable.SeqMap.empty

  override val f: PartialFunction[In, Out] = {
    case in: In =>
      skill.act(in) match {
        case evt: Event =>
          skill.commit(evt)
          evt
        case other => other
      }
  }

  def read(id: ID): Out = read.act(Read(id))

  private val read: RO = RO {
    case Read(id) => state.get(id) match {
      case Some(found) => Readen(id, found)
      case None => NotFound(id)
    }
  }

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

  private val find: Skill = RO {
    case Find(All) => Found(All, state.toMap) //TODO filter predicate
  }


  private val commit: RW = Skill(
    act = {
      case Apply(commands) =>
        val result: Seq[(In, Out)] = Apply.collapse(commands).values.map { cmd =>
          cmd -> skill.act(cmd)
        }.toSeq
        Commit(result)
    },
    commit = {
      case Commit(delta, extra) =>
        val data = delta.map {
          case (id, evt) => id -> skill.commit(evt)
        }
        extra.foreach {
          case (_, evt: Event) => skill.commit(evt) //TODO log? ignore?
        }
        new Data(Index(Type.build[ID], Type.build[Data]), data)
    }
  )

  private val skill: RW = Skill(create, read, update, delete, find, commit)
}
