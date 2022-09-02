package dev.rudiments.hardcore

import dev.rudiments.hardcore.CRUD.{Evt, I, O}
import dev.rudiments.hardcore.Predicate.Anything

import java.lang
import java.lang.IllegalStateException
import scala.collection.mutable

case class Node(
  var self: Thing = Nothing,
  leafs: mutable.Map[ID, Thing] = mutable.Map.empty,
  branches: mutable.Map[ID, Node] = mutable.Map.empty,
  relations: mutable.Map[Location, Seq[Location]] = mutable.Map.empty,
  keyIs: Predicate = Text(1024),
  leafIs: Predicate = Anything
) extends AgentCrud {
  override def read(where: Location): O = {
    where match {
      case Root => Readen(this)

      case id: ID =>
        (leafs.get(id), branches.get(id)) match {
          case (Some(l), Some(b)) =>
            Conflict(Readen(l), Readen(b))
          case (Some(leaf), None) => Readen(leaf)
          case (None, Some(b)) => Readen(b)
          case (None, None) => NotExist
        }

      case path: Path =>
        branches.get(path.ids.head) match {
          case Some(node) => node.read(path.dropHead)
          case None => NotFound(path)
        }

      case _ =>
        NotImplemented
    }
  }

//  from.get(Root) match {
//    case Some(Identical) => Node.empty
//    case Some(Created(n: Node)) => n
//    case Some(Created(t: Thing)) => Node(t)
//    case Some(Updated(n1, n2: Node)) if n1 == Node.empty => n2
//    case None => Node.empty
//    case other => throw new IllegalArgumentException(s"What happened? $other")
//  }

  def rememberSelf(what: O): O = what match {
    case Identical => Identical
    case Created(n: Node) =>
      NotSupported
    case c@Created(thing) => if(tags().contains(Node.Self)) {
      AlreadyExist(this.self)
    } else {
      this.self = thing
      c
    }
    case u@Updated(old: Node, n: Node) =>
      this.self = n.self //TODO - update checks
      u
    case Updated(old: Thing, n: Node) =>
      NotSupported
    case Updated(old: Node, n: Thing) =>
      NotSupported
    case u@Updated(old, newby) => if(tags().contains(Node.Self)) {
      if(old == this.self) {
        this.self = newby
        u
      } else {
        Conflict(Readen(this.self), u)
      }
    } else {
      NotExist
    }
    case d@Deleted(old) => if(tags().contains(Node.Self)) {
      if(old == this.self) {
        this.self = Nothing
        d
      } else {
        Conflict(Readen(this.self), d)
      }
    } else {
      NotExist
    }
    case other =>
      NotSupported
  }

  def redirect(path: Path, what: O): O = {
    this.read(path.dropTail) match {
      case Readen(n: Node) =>
        n.read(path.last) match {
          case Readen(l: Node) => l.rememberSelf(what)
          case Readen(thing) => n.asContainer(path.last, what)
          case NotExist =>
            n.asContainer(path.last, what)
        }
      case err: Error => err
      case nf: NotFound => nf
      case NotExist =>
        NotExist
      case other => Conflict(other, what)
    }
  }

  def asContainer(id: ID, what: O): O = {
    this.read(id) match {
      case NotExist =>
        what match {
          case c@Created(n: Node) =>
            this.branches += id -> n
            c
          case c@Created(thing) =>
            this.leafs += id -> thing
            c
          case other => NotExist
        }

      case r@Readen(n: Node) =>
        what match {
          case c@Created(_) => AlreadyExist(n)
          case u: Updated => ???
          case d@Deleted(old) if old == n =>
              this.branches -= id
              d
          case d: Deleted => Conflict(r, d)
          case other => n.rememberSelf(other)
        }
      case r@Readen(thing) =>
        what match {
          case _: Created => AlreadyExist(thing)

          case u@Updated(old, t) if old == thing =>
            this.leafs += id -> t
            u
          case u: Updated => Conflict(r, u)

          case d@Deleted(old) if old == thing =>
            this.leafs -= id
            d
          case d@Deleted(old) => Conflict(r, d)

          case other => NotImplemented
        }
    }
  }

  override def remember(where: Location, what: O): O = {
    where match {
      case Root =>
        this.rememberSelf(what)
      case id: ID =>
        this.asContainer(id, what)
      case path: Path =>
        this.redirect(path, what)
      case other =>
        throw new IllegalArgumentException(s"Not supported location: $other")
    }
  }

  override def report(q: Query): O = q match {
    case Read => Readen(this)
    case f@Find(All) => Found(f, leafs.toMap)
    case lf@LookFor(All) => Found(lf, structure)
    case d@Dump(All) => Found(d, everything())
    case Prepare => NotImplemented
    case Verify => ???
    case other => NotImplemented
  }

  def commit(c: Commit): O = {
    val ordered = c.crud.keys.toSeq.sorted(Location)
    val output = ordered.map { l => l -> remember(l, c.crud(l)) }.toMap
    val errors = output.collect {
      case p@(_, _: Error) => p
      case p@(_, NotExist) => p
      case p@(_, _: NotFound) => p
    }

    if(errors.isEmpty) {
      Committed(c)
    } else {
      MultiError(errors) //TODO rollback?
    }
  }

  def << (in: I) : O = in match {
    case q: Query => this.report(q)
    case c: Commit => this.commit(c)
  }

  def everything(prefix: Location = Root): Map[Location, Thing] = {
    val s = Map[Location, Thing](Root -> this.selfCopy())
    val l = this.leafs.toMap
    val b = branches.map { case (k, v) => k -> v.selfCopy() }.toMap
    val deep = branches.flatMap { case (id, b) => b.everything(id) }.toMap
    val all = s ++ l ++ b ++ deep
    all.map { case (k, v) =>
      prefix / k -> v
    }
  }

  def everything[K <: Thing, V <: Thing](prefix: Location, f: K => V): Map[Location, V] = {
    everything(prefix).map { case (k, v: K) => k -> f(v) }
  }

  def reconcile(to: Node): Map[Location, O] = {
    val source = this.everything()
    val target = to.everything()
    val keys = (source.keySet ++ target.keySet).toSeq.sorted(Location)

    keys.map { k =>
      (source.get(k), target.get(k)) match {
        case (None, None) => throw new IllegalStateException("How this happen?")
        case (None, Some(incoming)) => k -> Created(incoming)
        case (Some(existing), Some(incoming)) if existing == incoming => k -> Identical
        case (Some(existing), Some(incoming)) if existing != incoming => k -> Updated(existing, incoming)
        case (Some(existing), None) => k -> Deleted(existing)
      }
    }.toMap
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

  def tags(): Set[Node.Tag] = { //TODO Agent on Tag + control of update
    Seq(
      (self != Nothing) -> Node.Self,
      branches.nonEmpty -> Node.Branches,
      leafs.nonEmpty -> Node.Leafs,
      (branches.nonEmpty || leafs.nonEmpty) -> Node.Container,
      relations.nonEmpty -> Node.Relations
    ).collect { case (true, n) => n }.toSet
  }

  def selfCopy(): Node = this.copy(
    leafs = mutable.Map.empty,
    branches = mutable.Map.empty
  )
}


object Node {
  sealed trait Tag {}
  case object Self extends Tag
  case object Container extends Tag
  case object Leafs extends Tag
  case object Branches extends Tag
  case object Relations extends Tag

  def empty: Node = Node(Nothing)

  def fromEventMap(from: Map[Location, O]): Node = {
    val initial = from.get(Root) match {
      case Some(Identical) => Node.empty
      case Some(Created(n: Node)) => n
      case Some(Created(t: Thing)) => Node(t)
      case Some(Updated(n1, n2: Node)) if n1 == Node.empty => n2
      case None => Node.empty
      case other => throw new IllegalArgumentException(s"What happened? $other")
    }
    val ordered = (from.keySet - Root).toSeq.sorted(Location)
    ordered.foldLeft(initial) { case (acc, l) =>
      from(l) match {
        case Identical => acc //DO nothing
        case evt: Evt =>
          acc.remember(l, evt) match {
            case _: Evt => acc
            case Identical => acc
            case err: Error => throw new IllegalStateException(s"Error while recreating node: $l -> $err")
            case other => throw new IllegalStateException(s"Unexpected while recreating node: $l -> $other")
          }
        case report: Report =>
          throw new IllegalArgumentException(s"Expecting CRUD event, got report $l -> $report")
        case other =>
          throw new IllegalArgumentException(s"Expecting CRUD event, got $l -> $other")
      }
    }
  }

  def fromMap(from: Map[Location, Thing]): Node = {
    val ordered = from.keys.toSeq.sorted(Location)

    ordered.foldLeft(Node.empty) { (n, l) =>
      if(l == Root) {
        n.self = from.get(l) match {
          case Some(nd: Node) => nd.self
          case Some(t: Thing) => t
          case None => Nothing
        }
        n
      } else {
        (from(l), n ? l) match {
          case (t, NotExist) =>
            n.remember(l, Created(t))
            n
          case (t, nf: NotFound) =>
            //TODO create empty nodes from nf.missing
            n.remember(l, Created(t))
            n
          case (t, other) =>
            throw new IllegalArgumentException(s"$other not supported")
        }
      }
    }
  }

  def partnership(prefix: Location, from: Map[String, Predicate]): Node = {
    Node(relations = mutable.Map(
      (ID("types") / "Partners") -> from.map { case (k, _) => prefix / k }.toSeq ))
  }

  def apply(self: Thing, leafs: Map[ID, Thing], branches: Map[ID, Node]): Node = new Node(self, mutable.Map.from(leafs), mutable.Map.from(branches))
  def apply(self: Thing, leafs: Map[ID, Thing]): Node = new Node(self, mutable.Map.from(leafs), mutable.Map.empty)
  def apply(self: Thing): Node = new Node(self, mutable.Map.empty, mutable.Map.empty)
}
