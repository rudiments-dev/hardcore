package dev.rudiments.hardcore

import dev.rudiments.hardcore.CRUD.{Evt, I, O}
import dev.rudiments.hardcore.Predicate.Anything

import scala.collection.mutable

case class Node(
  var self: Thing = Nothing,
  leafs: mutable.Map[ID, Thing] = mutable.Map.empty,
  branches: mutable.Map[ID, Node] = mutable.Map.empty,
  keyIs: Predicate = Text(1024),
  leafIs: Predicate = Anything
) extends AgentCrud {
  override def read(where: Location): O = where match {
    case Root => Readen(this)
    case id: ID =>
      (leafs.get(id), branches.get(id)) match {
        case (Some(_), Some(_)) =>
          throw new IllegalStateException(s"Have both branch and leaf by #$id")
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
          case Created(in: Node) =>
            if(in.keyIs == this.keyIs && in.leafIs == this.leafIs) {
              if(this.self == Nothing && in.self != Nothing) {
                this.self = in.self
                Created(in.self)
              } else {
                NotImplemented
              }
            } else {
              NotImplemented
            }
          case c@Created(t: Thing) if self != t =>
            if(self == Nothing) {
              self = t
              c
            } else {
              AlreadyExist(self)
            }
          case Updated(_, _: Node) =>
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
              case Created(d: Data) =>
                if(d == leaf) {
                  Identical
                } else {
                  AlreadyExist(leaf)
                }
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
              case Created(n: Node) =>
                if(branch.leafs.isEmpty && branch.branches.isEmpty) {
                  branches += id -> n
                  Updated(branch, n)
                } else if (
                  branch.leafIs == n.leafIs &&
                    branch.keyIs == n.keyIs &&
                    branch.self == n.self
                ) {
                  Identical
                } else if(
                  branch.self == Nothing &&
                    n.self != Nothing &&
                    branch.keyIs == n.keyIs &&
                    branch.leafIs == branch.leafIs
                ) {
                  branch.self = n.self
                  Created(n.self)
                } else {
                  AlreadyExist(branch)
                }
              case Created(_) =>
                AlreadyExist(branch)
              case u@Updated(old, m: Node) =>
                ???
              case u@Updated(old, t) =>
                ???
              case d@Deleted(m: Node) if m == branch =>
                branches -= id
                d
              case other => throw new IllegalArgumentException(s"Expecting CRUD Event, got $other")
            }
          case (None, None) =>
            what match {
              case c@Created(m: Node) =>
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
                val m = leafs.get(h) match {
                  case Some(s) =>
                    leafs -= h
                    Node(s)
                  case None => Node.empty
                }
                branches += h -> m
                m.remember(tail, c)
              case other =>
                NotFound(path)
            }
        }

      case other => throw new IllegalArgumentException(s"Not supported location: $other")
    }
  }

  /* ThatNode
   (leafs.toMap ++
    branches.map { case (id, n) => id.asInstanceOf[Location] -> n.self }
   ).toMap ++ Map(Root -> self)
 */

  def report(q: Query): O = q match {
    case Read => Readen(this)
    case f@Find(All) => Found(f, leafs.toMap)
    case lf@LookFor(All) => Found(lf, structure)
    case d@Dump(All) => Found(d, everything())
    case Prepare => NotImplemented
    case Verify => ???
    case other => NotImplemented
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

  def << (in: I) : O = in match {
    case c: Commit => this.commit(c)
    case q: Query => this.report(q)
  }

  def everything(prefix: Location = Root): Map[Location, Thing] = {
    val all = this.leafs.toMap[Location, Thing] ++ branches.flatMap { case (id, b) => b.everything(id) }
    prefix match {
      case Root => all
      case Unmatched => ???
      case _: ID    => all.map { case (k, v) => prefix / k -> v }
      case _: Path  => all.map { case (k, v) => prefix / k -> v }
    }
  }

  def everything[K <: Thing, V <: Thing](prefix: Location, f: K => V): Map[Location, V] = {
    everything(prefix).map { case (k, v: K) => k -> f(v) }
  }

  def compareWith(another: Node): Map[Location, Event] = {
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
            store ++= found.everything(k, v => Deleted(v))
          case (None, Some(loaded)) =>
            store ++= loaded.everything(k, v => Created(v))
          case (Some(found), Some(loaded)) if found != loaded =>
            store ++= found.compareWith(loaded).map { case (l, v) => k / l -> v }
        }
      }
      store.toMap
    }
  }

  def structure: Map[Location, Thing] = {
    val store = mutable.Map.empty[Location, Thing]
    this.branches.foreach { case (id, m) =>
      store += id -> m.copy(leafs = mutable.Map.empty, branches = mutable.Map.empty)
      store ++= m.structure.map { case (k, v) => id / k -> v }
    }
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

  override def toString: String = s"Node(key: $keyIs, leafs: $leafIs, total: ${leafs.size}/${branches.size})"
}


object Node {
  def empty: Node = Node(Nothing)

  def fromEventMap(from: Map[Location, Evt]): Node = {
    from.foldLeft(Node.empty) { case (acc, (path, evt)) =>
      acc.remember(path, evt) match {
        case err: Error =>
          throw new IllegalArgumentException(s"Error from event map: '$err'")
        case other => acc //OK
      }
    }
  }

  def fromMap(from: Map[Location, Thing]): Node = {
    from.foldLeft(Node.empty) { (acc, el) =>
      el._2 match {
        case t: Thing =>
          acc ? el._1 match {
            case Readen(mem: Node) if mem.self != t && mem.self == Nothing => acc.remember(el._1, Created(t))
            case Readen(mem: Node) if mem.self != t => acc.remember(el._1, Updated(mem.self, t))
            case Readen(mem: Node) if mem.self == t => //OK, self with self
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

  def leafs(prefix: Location, from: Map[String, Predicate]): Node = {
    Node(leafs = mutable.Map.from(from.map { case (k, v) => ID(k) -> Link(prefix / k, v) }))
  }

  def apply(self: Thing, leafs: Map[ID, Thing], branches: Map[ID, Node]): Node = new Node(self, mutable.Map.from(leafs), mutable.Map.from(branches))
  def apply(self: Thing, leafs: Map[ID, Thing]): Node = new Node(self, mutable.Map.from(leafs), mutable.Map.empty)
  def apply(self: Thing): Node = new Node(self, mutable.Map.empty, mutable.Map.empty)
}
