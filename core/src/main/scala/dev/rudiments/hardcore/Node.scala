package dev.rudiments.hardcore

import dev.rudiments.hardcore.Node.Cursor

import java.lang
import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.collection.mutable

case class Node(
  state: mutable.Map[Location, Product] = mutable.Map.empty
) {
  private def cursor() = new Node.Cursor(this)

  def read(l: Location): Out with CRUD = cursor().search(l).out

  def apply(l: Location, event: Event with CRUD): Out with CRUD = {
    cursor()
      .search(l)
      .check(event)
      .unsafeApply()
  }

  def apply(commit: Commit): Out with CRUD = {
    val (errors, events) = commit.flatten
      .map { (l, evt) => l -> cursor().search(l).check(evt) }
      .partitionMap { (l, c) => c.out match
        case _: Event with CRUD => Right(l -> c)
        case _ => Left(l -> c.out)
      }

    if (errors.isEmpty) {
      try {
        val executed = events
          .map { (l, cur) => l -> cur.unsafeApply() } //TODO rollback if errors
          .filter { _._2 match
              case _: Event with CRUD => false
              case _ => true
          }
        if(executed.isEmpty) commit
        else MultiError(executed:_*)
      } catch {
        case e: Exception => InternalError(e)
      }
    } else {
      MultiError(errors:_*)
    }
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
}

object Node {
  def empty: Node = new Node()

  def from(c: Commit): Either[MultiError, Node] = {
    val node = Node.empty
    node.apply(c) match {
      case _: Commit => Right(node)
      case m: MultiError => Left(m)
      case other =>
        throw new IllegalArgumentException(s"Should never happen: $other")
    }
  }

  final private class Cursor(starting: Node) {
    var node: Node = starting
    var to: List[ID] = Nil
    var in: List[ID] = Nil
    var out: Out with CRUD = _

    def search(l: Location): Cursor = {
      searchIds(l.toIds.toList)
      this
    }

    @tailrec
    private def searchIds(ids: List[ID]): Unit = ids match {
      case Nil => node.state.get(Self) match
        case Some(v) => out = Readen(v)
        case None => out = NotFound(Self)
      case id :: Nil => node.state.get(id) match
        case Some(n: Node) =>
          node = n
          to = id +: to
          out = Readen(n)
        case Some(v) =>
          in = id :: Nil
          out = Readen(v)
        case None =>
          in = id :: Nil
          out = NotFound(id)
      case h :: t => node.state.get(h) match
        case Some(n: Node) =>
          node = n
          to = h +: to
          searchIds(t)
        case Some(_) =>
          in = h :: Nil
          out = NotSupported(Read(Location(t:_*)))
        case None =>
          in = Nil
          out = NotFound(Location(ids:_*))
    }

    def inLocation: Location = Location(in:_*)

    def location: Location = Location(to ++ in: _*)

    def check(event: Event with CRUD): Cursor = {
      out = (out, event) match
        case (NotFound(_), c: Created) => c
        case (Readen(v), u@Updated(v1, v2)) if v == v1 && v1 != v2 => u
        case (r@Readen(v), Updated(v1, v2)) if v == v1 && v1 == v2 => r
        case (Readen(v), d@Deleted(old)) if v == old => d
        case (Readen(_), c: Commit) =>
          val errors = c.flatten
            .map { (l, evt) => l -> node.cursor().search(l).check(evt).out }
            .filter { _._2 match
                case _: Event with CRUD => false
                case _ => true
            }

          if (errors.isEmpty) {
            c
          } else {
            MultiError(errors: _*)
          }
        case (nf: NotFound, _) => nf
        case (actual, event) => Conflict(event, actual)

      this
    }

    def unsafeApply(): Out with CRUD = {
      out match
        case c@Created(v) => node.state += (inLocation -> v); c
        case u@Updated(_, v) => node.state += (inLocation -> v); u
        case d: Deleted => node.state -= inLocation; d
        case c: Commit => node.apply(c); c
        case other => other
    }
  }
}