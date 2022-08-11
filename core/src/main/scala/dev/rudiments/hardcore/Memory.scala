package dev.rudiments.hardcore

import dev.rudiments.hardcore.CRUD.{Evt, I, O}
import dev.rudiments.hardcore.Predicate.{All, Anything}

import scala.collection.mutable

case class Memory(
  var self: Thing = Nothing,
  leafs: mutable.Map[ID, Thing] = mutable.Map.empty,
  branches: mutable.Map[ID, Memory] = mutable.Map.empty,
  keyIs: Predicate = Text(1024),
  leafIs: Predicate = Anything
) extends AgentCrud {
  override def read(where: Location): O = where match {
    case Root => Readen(this)
    case id: ID =>
      (leafs.get(id), branches.get(id)) match {
        case (Some(_), Some(_)) => throw new IllegalStateException(s"Have both branch and leaf by #$id")
        case (Some(leaf), None) => Readen(leaf)
        case (None, Some(b)) => Readen(b)
        case (None, None) => NotExist
      }
    case path: Path => branches.get(path.ids.head) match {
      case Some(node) => node ? (path -/ path.ids.head)
      case None => NotFound(where)
    }
    case _ => throw new IllegalArgumentException("Not supported")
  }

  override def remember(where: Location, what: O): O = {
    where match {
      case Root =>
        what match {
          case Created(_: Memory) =>
            throw new IllegalArgumentException("Not supported")
          case c@Created(t: Thing) if self != t =>
            if(self == Nothing) {
              self = t
              c
            } else {
              AlreadyExist(self)
            }
          case Updated(_, _: Memory) =>
            throw new IllegalArgumentException("Not supported")
          case u@Updated(old, t) =>
            if (self != t && self == old) { //TODO seen update from (main, test) to (scala, resources) means some folder self-updating with children
              self = t
              u
            } else if (self ==t) {
              Identical
            } else {
              Conflict(Readen(self), u)
            }

          case d@Deleted(t) =>
            if(self != Nothing && self == t) {
              self = Nothing
              d
            } else {
              Conflict(Readen(self), d)
            }
          case other => throw new IllegalArgumentException(s"Expecting CRUD Event, got $other")
        }

      case id: ID =>
        (leafs.get(id), branches.get(id)) match {
          case (Some(_), Some(_)) => throw new IllegalStateException(s"Have both leaf and branch by $id")
          case (Some(leaf), None) =>
            what match {
              case Created(_) =>
                AlreadyExist(leaf)
              case u@Updated(old, t) if old == leaf =>
                leafs += id -> t
                u
              case d@Deleted(old) if old == leaf =>
                leafs -= id
                d
              case other => throw new IllegalArgumentException(s"Expecting CRUD Event, got $other")
            }
          case (None, Some(branch)) =>
            what match {
              case Created(d: Data) =>
                if(branch.self != Nothing) {
                  val old = branch.self
                  branch.self = d
                  Updated(old, d)
                } else {
                  branch.self = d
                  Created(d)
                }
              case Created(Nothing) =>
                if (branch.self == Nothing) {
                  Identical
                } else {
                  Conflict(Created(Nothing), Readen(Nothing))
                }
              case Created(_) =>
                AlreadyExist(branch)
              case u@Updated(old, m: Memory) => ???
              case u@Updated(old, t) => ???
              case d@Deleted(m: Memory) if m == branch =>
                branches -= id
                d
              case other => throw new IllegalArgumentException(s"Expecting CRUD Event, got $other")
            }
          case (None, None) =>
            what match {
              case c@Created(m: Memory) =>
                branches += id -> m
                c
              case c@Created(t) =>
                leafs += id -> t
                c
              case other => NotExist
            }
        }

      case path: Path =>
        val h = path.ids.head
        val tail = path -/ h
        branches.get(h) match {
          case Some(found) => found.remember(tail, what)
          case None =>
            what match {
              case c: Created =>
                val m = Memory.empty
                branches += h -> m
                m.remember(tail, c)
              case other =>
                NotFound(path)
            }
        }

      case other => throw new IllegalArgumentException(s"Not supported location: $other")
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

  def flatten(): Map[Location, Thing] = structure ++ allLeafs

  def compareWith(another: Memory): Map[Location, Event] = {
    if (this == another) {
      Map.empty
    } else {
      val store = mutable.Map.empty[Location, Event]

      if (this.self != another.self) {
        if(another.self == Nothing) {
          store += Root -> Deleted(this.self)
        } else if(this.self == Nothing) {
          store += Root -> Created(another.self)
        } else {
          store += Root -> Updated(this.self, another.self)
        }
      }

      val leafKeys = this.leafs.keys ++ another.leafs.keys
      leafKeys.foreach { k =>
        (this.leafs.get(k), another.leafs.get(k)) match {
          case (None, None) => throw new IllegalArgumentException("How this happened?")
          case (Some(found), None) =>
            store += k -> Deleted(found)
          case (None, Some(loaded)) =>
            store += k -> Created(loaded)
          case (Some(found), Some(loaded)) if found != loaded =>
            store += k -> Updated(found, loaded)
        }
      }

      val branchKeys = this.branches.keys ++ another.branches.keys
      branchKeys.foreach { k =>
        (this.branches.get(k), another.branches.get(k)) match {
          case (None, None) => throw new IllegalArgumentException("How this happened?")
          case (Some(found), None) =>
            store ++= found.flatten().map { case (l, v) => k / l -> Deleted(v) }
          case (None, Some(loaded)) =>
            store ++= loaded.flatten().map { case (l, v) => k / l -> Created(v) }
          case (Some(found), Some(loaded)) if found != loaded =>
            store ++= found.compareWith(loaded).map { case (l, v) => k / l -> v }
        }
      }
      store.toMap
    }
  }

  def structure: Map[Location, Thing] = {
    val store = mutable.Map.empty[Location, Thing]
    if(this.self != Nothing) {
      store += Root -> this.self
    }
    this.branches.foreach { case (id, m) =>
      store ++= m.structure.map { case (k, v) => id / k -> v }
    }

    store.toMap
  }

  def allLeafs: Map[Location, Thing] = {
    val store = mutable.Map.empty[Location, Thing]
    store ++= this.leafs
    store ++= branches.flatMap { case (id, b) => b.allLeafs.map { case (k, v) => id / k -> v }}
    store.toMap
  }

  def decodeAndReadLocation(s: Seq[String]): (Location, Thing) = s.size match {
    case 0 => (Root, this)
    case 1 =>
      val id = decodeKey(s.head)
      (id, read(id))
    case _ =>
      val id = decodeKey(s.head)
      branches.get(id) match {
        case Some(found) =>
          val rec = found.decodeAndReadLocation(s.tail)
          id / rec._1 -> rec._2 //some pattern are looking at me, like `id / (pair) => id / pair._1 -> pair._2`
        case None => Location(s) -> NotFound(Location(s))
      }
  }

  private def decodeKey(s: String): ID = keyIs match {
    case Text(_) => ID(s)
    case Number(Long.MinValue, Long.MaxValue) => ID(s.toLong)
    case other => throw new IllegalArgumentException(s"Not supported key decoding: $other")
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
            case other => //OK
          }
          acc
        case t: Thing =>
          acc ? el._1 match {
            case Readen(mem: Memory) if mem.self != t => acc.remember(el._1, Updated(mem.self, t))
            case Readen(mem: Memory) if mem.self == t => //OK, self with self
            case Readen(Nothing) if t == Nothing =>
            //OK, nothing with nothing
            case Readen(_: Thing) =>
              throw new IllegalArgumentException(s"Error from map: '${el._1} already exist'")
            case NotExist => acc.remember(el._1, Created(t))
            case NotFound(_) => acc.remember(el._1, Created(t))
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

  def apply(self: Thing, leafs: Map[ID, Thing], branches: Map[ID, Memory]): Memory = new Memory(self, mutable.Map.from(leafs), mutable.Map.from(branches))
  def apply(self: Thing, leafs: Map[ID, Thing]): Memory = new Memory(self, mutable.Map.from(leafs), mutable.Map.empty)
  def apply(self: Thing): Memory = new Memory(self, mutable.Map.empty, mutable.Map.empty)
}
