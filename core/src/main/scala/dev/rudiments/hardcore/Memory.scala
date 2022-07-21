package dev.rudiments.hardcore

import dev.rudiments.hardcore.CRUD.{Evt, I, O}
import dev.rudiments.hardcore.Memory.commits
import dev.rudiments.hardcore.Predicate.All

import scala.annotation.tailrec
import scala.collection.mutable

case class Memory(
  total: mutable.Map[Location, mutable.Seq[Evt]] = mutable.Map.empty,
  last: mutable.Map[Location, Evt] = mutable.Map.empty,
  node: MemoryNode = new MemoryNode()
) extends AgentCrud {

  node ? commits match {
    case NotExist => node += commits -> new MemoryNode()
  }

  private def unsafeUpdateState(where: Location, what: Evt): Evt = {
    last.get(where) match {
      case Some(_) =>
        last += where -> what
        total(where).+:(what)
        node.remember(where, what)
        what
      case None =>
        last += where -> what
        total += where -> mutable.Seq(what)
        node.remember(where, what)
        what
    }
  }

  override def read(where: Location): O = last.get(where) match {
    case Some(Created(found)) => Readen(found)
    case Some(Updated(_, found)) => Readen(found)
    case Some(Deleted(_)) => NotExist
    case Some(Committed(_)) => ???
    case None => NotExist
  }

  override def remember(where: Location, via: O): O = {
    (this ? where, via) match {
      case (NotExist, c: Created)                  => unsafeUpdateState(where, c)
      case (Readen(r), Created(_))                 => AlreadyExist(r)
      case (Readen(r), Updated(u, data)) if r == u => unsafeUpdateState(where, Updated(r, data))
      case (Readen(r), Deleted(d)) if r == d       => unsafeUpdateState(where, Deleted(r))
      case (found, other)                          => Conflict(found, other)
    }
  }

  override def find(where: Location, p: Predicate): O = node.find(where, p)

  def commit(c: Commit): O = {
    val outputs = c.crud
      .map { case (id, evt: Evt) =>
        id -> remember(id, evt)
      }

    val crudErrors = outputs.collect {
        case (id, err: Error) => (id, err)
        case (id, NotExist) => (id, NotExist)
      }

    val p = commits / ID(c.hashCode().toString)
    val errors = this += p -> c match {
      case _: Created => crudErrors
      case m: MultiError => crudErrors + (p -> m)
      case e: Error => crudErrors + (p -> e)
      case other => crudErrors + (p -> Conflict(c, other))
    }

    if(errors.isEmpty) {
      Committed(c)
    } else {
      MultiError(errors) //TODO rollback?
    }
  }

  def execute(in: I): O = in match {
    case c: Commit => commit(c)
    case Find(All) => Found(All, node.find())
    case _ => NotImplemented
  }

  def << (in: I) : O = this.execute(in)
}

object Memory {
  val commits: ID = ID("commits")

  val reducer: PartialFunction[(Evt, Evt), O] = {
    case (   Created(c1),      Created(_))                 => AlreadyExist(c1)
    case (   Created(c1),    u@Updated(u2, _)) if c1 == u2 => u
    case (   Created(c1),    d@Deleted(d2))    if c1 == d2 => d

    case (   Updated(_, u1),   Created(_))                 => AlreadyExist(u1)
    case (   Updated(_, u1), u@Updated(u2, _)) if u1 == u2 => u
    case (   Updated(_, u1), d@Deleted(d2))    if u1 == d2 => d

    case (   Deleted(_),     c@Created(_))                 => c
    case ( d@Deleted(_),     u@Updated(_, _))              => AlreadyNotExist(d, u)
    case (d1@Deleted(_),    d2@Deleted(_))                 => AlreadyNotExist(d1, d2)
    case (that, other) /* unfitting updates */             => Conflict(that, other)
  }
}

class MemoryNode extends AgentCrud {
  var self: Thing = Nothing
  val leafs: mutable.Map[ID, Thing] = mutable.Map.empty
  val branches: mutable.Map[ID, MemoryNode] = mutable.Map.empty

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
          case Readen(m: MemoryNode) =>
            var p: Location = Root
            path.ids.dropRight(1).foreach { id =>
              p = p / id
              m.unsafeUpdateState(p, Created(new MemoryNode))
            }
          case other => throw new IllegalArgumentException(s"unexpected $other")
        }
        unsafeUpdateState(where, c)

      case (NotExist, c: Created)                  => unsafeUpdateState(where, c)
      case (Readen(r), Created(_))                 => AlreadyExist(r)
      case (Readen(r), Updated(u, data)) if r == u => unsafeUpdateState(where, Updated(r, data))
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
    case Readen(n: MemoryNode) => Found(All, n.find())
    case r: Readen => Conflict(r, Find(p))
    case other => other
  }

  def find(): Map[Location, Thing] = {
    if(this.self != Nothing) {
      (Map(Root -> self) ++ branches.toMap.flatMap { case (id, b) =>
        b.find().map { case (k, v) => id / k -> v }
      } ++ leafs.toMap).toMap
    } else {
      branches.toMap.flatMap { case (id, b) =>
        b.find().map { case (k, v) => id / k -> v }
      } ++ leafs.toMap
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
    case (id: ID, Created(mem: MemoryNode)) =>
      this.branches += id -> mem
      what
    case (id: ID, Created(data)) =>
      this.leafs += id -> data
      what
    case (Root, Created(t)) if self == Nothing =>
      self = t
      what
    case (id: ID, Updated(_, mem: MemoryNode)) =>
      this.branches += id -> mem
      what
    case (id: ID, Updated(_, data)) =>
      this.leafs += id -> data
      what
    case (Root, Updated(old, t)) if self == old =>
      self = t
      what
    case (id: ID, Deleted(_: MemoryNode)) =>
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
object MemoryNode {
  def empty: MemoryNode = new MemoryNode()
  def wrap(l: Location, t: Thing): MemoryNode = {
    val node = new MemoryNode()
    l match {
      case Root =>
        node += Root -> t
      case id: ID =>
        node += id -> t
      case path: Path =>
        val h = path.ids.head
        node += h -> wrap(path -/ h, t)

      case other => throw new IllegalArgumentException(s"$other not supported in Node")
    }
    node
  }
  def fromMap(from: Map[Location, Thing]): MemoryNode = {
    from.foldLeft(MemoryNode.empty) { (acc, el) =>
      el._2 match {
        case o: O =>
          acc.remember(el._1, o)
          acc
        case other => //TODO do not ignore errors
          throw new IllegalArgumentException(s"Wrong type of thing: $other")
      }
    }
  }
}