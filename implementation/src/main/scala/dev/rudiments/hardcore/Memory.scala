package dev.rudiments.hardcore


import scala.collection.mutable

abstract class Agent(val in: Predicate, val out: Predicate) extends PartialFunction [In, Out] {
  val f: PartialFunction[In, Out]
  override def isDefinedAt(x: In): Boolean = f.isDefinedAt(x)
  override def apply(x: In): Out = f.apply(x)
}

class Memory(override val in: Predicate = Type.build[In], override val out: Predicate = Type.build[Out]) extends Agent(in, out) {
  val state: mutable.SeqMap[ID, Data] = mutable.SeqMap.empty

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

  private val find: Skill = Skill(
    act = {
      case Find(All) => Found(All, state.toMap) //TODO filter predicate
    },
    commit = { //TODO remove reaction if In is Query?
      case Found(_, found) => found.head._2
    }
  )

  private val commit: Skill = Skill(
    act = {
      case Apply(commands) =>
        val result: Seq[(Command, Event)] = commands
          .map { cmd => cmd -> skill.act(cmd) }
          .collect { case it: (Command, Event) => it } //TODO not hide failures? or propagate errors?
        Commit(result)
    },
    commit = {
      case Commit(events) =>
        val prepare = events.collect {
          case (_, crud: CRUD) => crud.id -> crud
        }.foldLeft(Map.empty[ID, Event]) {
          (acc, evt) =>
            val r = acc.get(evt._1) match {
              case None =>
                evt._2 match { case crud: Event with CRUD => crud.id -> crud }
              case Some(found) =>
                (found, evt._2) match {
                  case (Created(id1, d1), Updated(id2, _, d22)) if id1 == id2 && d1 == d22 => id1 -> Created(id1, d22)
                  case (Updated(id1, d11, d12), Updated(id2, d21, d22)) if id1 == id2 && d12 == d21 => id1 -> Updated(id1, d11, d22)
                  case (Created(id1, d1), Deleted(id2, d2)) if id1 == id2 && d1 == d2 => id1 -> Deleted(id1, d1)
                  case (Updated(id1, _, d12), Deleted(id2, d2)) if id1 == id2 && d12 == d2 => id1 -> Deleted(id1, d12)
                  case (_, _) => throw new IllegalArgumentException("Conflict")
                }
            }
            acc + r
        }
        val data = prepare.map { case (id, evt) => id -> skill.commit(evt) }
        new Data(Index(Type.build[ID], Type.build[Data]), data)
    }
  )

  private val skill: Skill = Skill(
    act = read.act.orElse(create.act).orElse(update.act).orElse(delete.act).orElse(find.act).orElse(commit.act),
    commit = create.commit.orElse(update.commit).orElse(delete.commit).orElse(commit.commit)
  )
}
