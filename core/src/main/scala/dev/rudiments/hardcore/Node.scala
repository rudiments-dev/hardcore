package dev.rudiments.hardcore

import java.lang
import scala.collection.immutable.ListMap
import scala.collection.mutable

class Node(
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

  def apply(key: Location, event: Event with CRUD): Out with CRUD = {
    readChain(key, event) match {
      case evt: Event with CRUD => unsafeApply(key, evt)
      case other => other
    }
  }

  def unsafeApply(key: Location, event: Event with CRUD): Event with CRUD = event match {
    case c@Created(v) => state += (key -> v); c
    case u@Updated(_, v) => state += (key -> v); u
    case d: Deleted => state -= key; d
    case other => throw new IllegalArgumentException(s"Not event: $other")
  }

  def apply(commit: Commit): Out = { // TODO merge with apply(location, event)
    val (errors, events) = commit.cud
      .map { case (l, evt) => l -> readChain(l, evt) }
      .partitionMap {
        case (l, evt: Event with CRUD) => Right(l -> evt)
        case p => Left(p)
      }

    if (errors.isEmpty) {
      val executed = events.map { case (l, evt) => l -> this.unsafeApply(l, evt) }
      Applied(Commit(executed:_*))
    } else {
      MultiError(errors:_*)
    }
  }

  def readChain(l: Location, after: Event with CRUD): Out with CRUD = (read(l), after) match {
    case (NotFound(_), c: Created) => c
    case (Readen(v), u@Updated(v1, v2)) if v == v1 && v1 != v2 => u
    case (r@Readen(v), Updated(v1, v2)) if v == v1 && v1 == v2 => r
    case (Readen(v), d@Deleted(old)) if v == old => d
    //TODO commit
    case (actual, event) => Conflict(event, actual)
  }

  def chain(before: Out with CRUD, after: Event with CRUD): Out with CRUD = (before, after) match {
    case (NotFound(_), c: Created) => c
    case (Deleted(_), c: Created) => c
    case (Readen(v), u@Updated(v1, v2)) if v == v1 && v1 != v2 => u
    case (Created(v), u@Updated(v1, v2)) if v == v1 && v1 != v2 => u
    case (Updated(_, v), u@Updated(v1, v2)) if v == v1 && v1 != v2 => u
    case (Readen(v), d@Deleted(old)) if v == old => d
    case (Created(v), d@Deleted(old)) if v == old => d
    case (Updated(_, v), d@Deleted(old)) if v == old => d
    //TODO commit
    case (actual, event) => Conflict(event, actual)
  }

  def size: Int = this.state.size
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