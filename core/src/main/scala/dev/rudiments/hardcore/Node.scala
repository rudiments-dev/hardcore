package dev.rudiments.hardcore

import java.lang
import scala.collection.immutable.ListMap
import scala.collection.mutable

case class Node(
  state: mutable.Map[Location, Product] = mutable.Map.empty
) {
  def read(key: Location): Out with CRUD = {
    key match {
      case Self | ID(_) => state.get(key) match {
        case Some(v) => Readen(v)
        case None => NotFound(key)
      }
      case p: Path =>
        state.get(p.head) match {
          case Some(node: Node) => node.read(p.tail) //TODO optimize if needed
          case Some(_) => NotSupported(Read(p.tail))
          case None => NotFound(p)
        }
    }
  }

  def tread(l: Location): (Node, Location, Out with CRUD) = l match
    case Self | ID(_) => state.get(l) match
      case Some(node: Node) => (node, Self, Readen(node))
      case Some(v) => (this, l, Readen(v))
      case None => (this, l, NotFound(l))
    case p: Path => state.get(p.head) match
      case Some(node: Node) =>
        val readen = node.tread(p.tail)
        (readen._1, p.head / readen._2, readen._3)
      case Some(_) => (this, l, NotSupported(Read(p.tail)))
      case None => (this, l, NotFound(p))


  def apply(key: Location, event: Event with CRUD): Out with CRUD = {
    readChain(key, event) match {
      case (node, evt: Event with CRUD) => node.unsafeApply(key, evt)
      case (_, other) => other
    }
  }

  def unsafeApply(key: Location, event: Event with CRUD): Event with CRUD = event match {
    case c@Created(v) => state += (key -> v); c
    case u@Updated(_, v) => state += (key -> v); u
    case d: Deleted => state -= key; d
    case other => throw new IllegalArgumentException(s"Not event: $other")
  }

  def apply(commit: Commit): Out with CRUD = { // TODO merge with apply(location, event)
    val (errors, events) = commit.cud
      .map { case (l, evt) => l -> readChain(l, evt) }
      .partitionMap {
        case (l, (node, evt: Event with CRUD)) => Right((node, l, evt))
        case (l, (_, o)) => Left(l -> o)
      }

    if (errors.isEmpty) {
      val executed = events.map { case (node, l, evt) => l -> node.unsafeApply(l, evt) }
      Applied(Commit(executed:_*))
    } else {
      MultiError(errors:_*)
    }
  }

  def readChain(
    l: Location, after: Event with CRUD
  ): (Node, Out with CRUD) = (tread(l), after) match {
    case ((n, _, NotFound(_)), c: Created) => n -> c
    case ((n, _, Readen(v)), u@Updated(v1, v2)) if v == v1 && v1 != v2 => n -> u
    case ((n, _, r@Readen(v)), Updated(v1, v2)) if v == v1 && v1 == v2 => n -> r
    case ((n, _, Readen(v)), d@Deleted(old)) if v == old => n -> d
    case ((_, _, Readen(node: Node)), c: Commit) => node -> c
    case ((n, _, actual), event) => n -> Conflict(event, actual)
  }

  def size: Int = this.state.size

  def >+ (pair: (Location, Product)): Out with CRUD = this.apply(pair._1, Created(pair._2))
  def >* (pair: (Location, Product)): Out with CRUD = this.read(pair._1) match {
    case Readen(r) => this.apply(pair._1, Updated(r, pair._2))
    case other => other
  }
  def >- (l: Location): Out with CRUD = this.read(l) match {
    case Readen(r) => this.apply(l, Deleted(r))
    case other => other
  }

  def >> (pairs: (Location, Event with CRUD)*): Out with CRUD = this.apply(Commit(pairs:_*))
}

object Node {
  def empty: Node = new Node()

  def from(c: Commit): Either[MultiError, Node] = {
    val node = Node.empty
    node.apply(c) match {
      case _: Applied => Right(node)
      case m: MultiError => Left(m)
      case other =>
        throw new IllegalArgumentException(s"Should never happen: $other")
    }
  }
}