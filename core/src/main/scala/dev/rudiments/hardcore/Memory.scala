package dev.rudiments.hardcore

import dev.rudiments.hardcore.CRUD.{Evt, I, O}
import dev.rudiments.hardcore.Predicate.All

import scala.annotation.tailrec
import scala.collection.mutable

case class Memory(
  var self: Thing = Nothing,
  leafs: mutable.Map[ID, Thing] = mutable.Map.empty,
  branches: mutable.Map[ID, Memory] = mutable.Map.empty
) extends AgentCrud {
  override def read(where: Location): O = where match {
    case Root => Readen(this)
    case id: ID =>
      leafs.get(id).map(Readen)
        .getOrElse(branches.get(id).map(Readen)
          .getOrElse(NotExist))
    case path: Path => branches.get(path.ids.head) match {
      case Some(node) => node ? (path -/ path.ids.head)
      case None => NotFound(where)
    }
    case _ => throw new IllegalArgumentException("Not supported")
  }

  override def remember(where: Location, what: O): O = {
    val readen = this ? where
    (readen, what) match {
      case (NotFound(_: ID), c: Created)           => unsafeUpdateState(where, c)
      case (NotFound(path: Path), c: Created)      =>
        val head = where.asInstanceOf[Path] /- path
        this ? head match {
          case Readen(m: Memory) =>
            var p: Location = Root
            path.ids.dropRight(1).foreach { id =>
              p = p / id
              m.unsafeUpdateState(p, Created(Memory.empty))
            }
          case Readen(t: Thing) =>
            val mem = Memory(self = t)
            var p: Location = Root
            path.ids.dropRight(1).foreach { id =>
              p = p / id
              mem.unsafeUpdateState(p, Created(Memory.empty))
            }
            this.unsafeUpdateState(head, Created(mem))
          case other => throw new IllegalArgumentException(s"unexpected $other")
        }
        unsafeUpdateState(where, c)

      case (NotExist, c: Created)                  => unsafeUpdateState(where, c)
      case (Readen(t: Thing), Created(m: Memory)) =>
        if(m.self == Nothing) {
          m.self = t
          Created(m)
        } else {
          Updated(t, m)
        }
      case (Readen(m: Memory), Created(t: Thing)) =>
        if(m.self == Nothing) {
          m.self = t
          Created(t)
        } else {
          val old = m.self
          m.self = t
          Updated(old, t)
        }
      case (Readen(r), Created(_))                 => AlreadyExist(r)
      case (Readen(r), Updated(u, data)) if r == u => unsafeUpdateState(where, Updated(r, data))
      case (Readen(m: Memory), u@Updated(old: Thing, t: Thing)) if old == m.self =>
        m.self = t
        u
      case (Readen(r), Deleted(d)) if r == d       => unsafeUpdateState(where, Deleted(r))
      case (found, other)                          => Conflict(found, other)
    }
  }

  def execute(in: I): O = in match {
    case c: Commit => commit(c)
    case Find(All) => Found(All, find())
    case _ => NotImplemented
  }

  override def find(where: Location, p: Predicate): O = this ? where match {
    case Readen(n: Memory) => Found(All, n.find())
    case r: Readen => Conflict(r, Find(p))
    case other => other
  }

  def find(): Map[Location, Thing] = {
    val b: Map[Location, Thing] = branches.toMap.flatMap { case (id, b) =>
      val found: Map[Location, Thing] = b.find()
      found.map {
        case (Root, v) => id -> v
        case (k, v) => id / k -> v
      }
    }
    if(this.self != Nothing) {
      b + (Root -> self) ++ leafs.toMap
    } else {
      b ++ leafs.toMap
    }
  }

  def commit(c: Commit): O = {
    val errors = c.crud
      .map { case (id, evt: Evt) => id -> remember(id, evt) }
      .collect { case (id, err: Error) => (id, err) }

    if(errors.isEmpty) {
      Committed(c)
    } else {
      MultiError(errors) //TODO rollback?
    }
  }

  def << (in: I) : O = this.execute(in)

  @tailrec
  private def unsafeUpdateState(where: Location, what: O): O = (where, what) match {
    case (id: ID, Created(mem: Memory)) =>
      this.branches += id -> mem
      what
    case (id: ID, Created(data)) =>
      this.leafs += id -> data
      what
    case (Root, Created(t)) if self == Nothing =>
      self = t
      what
    case (id: ID, Updated(_, mem: Memory)) =>
      this.branches += id -> mem
      what
    case (id: ID, Updated(_, data)) =>
      this.leafs += id -> data
      what
    case (Root, Updated(old, t)) if self == old =>
      self = t
      what
    case (id: ID, Deleted(_: Memory)) =>
      branches -= id
      what
    case (id: ID, Deleted(_)) =>
      leafs -= id
      what
    case (Root, Deleted(old)) if self == old =>
      self = Nothing
      what
    case (p: Path, evt: O) =>
      branches.get(p.ids.head) match {
        case Some(node) => node.unsafeUpdateState(p -/ p.ids.head, evt)
        case None => NotFound(p)
      }

    case (other, another) => MultiError(Map(other -> another))
  }
}
object Memory {
  def empty: Memory = Memory(Nothing)
  def wrap(l: Location, t: Thing): Memory = (l, t) match {
    case (Root, _: Memory) => throw new IllegalArgumentException("Memory node in root is forbidden")
    case (Root, thing) => Memory(thing)
    case (id: ID, mem: Memory) => Memory(branches = mutable.Map(id -> mem))
    case (id: ID, thing) => Memory(leafs = mutable.Map(id -> thing))
    case (path: Path, _) =>
      Memory(branches = mutable.Map(path.ids.head -> wrap(path -/ path.ids.head, t)))
    case other => throw new IllegalArgumentException(s"$other not supported in MemoryNode")
  }

  def fromMap(from: Map[Location, Thing]): Memory = {
    from.foldLeft(Memory.empty) { (acc, el) =>
      el._2 match {
        case o: O =>
          acc.remember(el._1, o) match {
            case err: Error => throw new IllegalArgumentException(s"Error from map: '$err'")
          }
          acc
        case other => //TODO do not ignore errors
          throw new IllegalArgumentException(s"Wrong type of thing: $other")
      }
    }
  }

  def leafs(prefix: Location, from: Map[String, Predicate]): Memory = {
    Memory(leafs = mutable.Map.from(from.map { case (k, v) => ID(k) -> Link(prefix / k, v) }))
  }
}
