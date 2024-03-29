package dev.rudiments.hardcore

import dev.rudiments.hardcore.CRUD.{I, O}

trait AgentCrud extends Agent {

  def ask(where: Location, what: I): O = (read(where), what) match {
    case (NotExist, Create(data)) => Created(data)
    case (NotFound(_), Create(data)) => Created(data)
    case (NotExist, _) => NotExist
    case (n: NotFound, _) => n
    case (r: Readen, Read) => r
    case (Readen(found), Create(_)) => AlreadyExist(found)
    case (Readen(found), Update(data)) => Updated(found, data)
    case (Readen(found), Delete) => Deleted(found)
    case (other, another) =>
      Conflict(other, another)
  }

  def remember(where: Location, what: O): O

  def + (pair: (Location, Thing)): O = this.ask(pair._1, Create(pair._2))
  def * (pair: (Location, Thing)): O = this.ask(pair._1, Update(pair._2))
  def - (where: Location): O = this.ask(where, Delete)

  def += (pair: (Location, Thing)): O = this + pair match {
    case c: Created => this.remember(pair._1, c)
    case other => other
  }

  def *= (pair: (Location, Thing)): O = this * pair match {
    case u: Updated => this.remember(pair._1, u)
    case other => other
  }
  def -= (where: Location): O = this - where match {
    case d: Deleted => this.remember(where, d)
    case other => other
  }

  def := (pair: (Location, Thing)): O = (read(pair._1), pair._2) match {
    case (NotExist, Nothing) => Conflict(NotExist, Delete)
    case (NotExist, t) => this.remember(pair._1, Created(t))
    case (NotFound(_), t) => this.remember(pair._1, Created(t))
    case (Readen(r), t) if r != t => this.remember(pair._1, Updated(r, t))
    case (r@Readen(r1), t) if r1 == t => r
    case (Readen(r), Nothing) => this.remember(pair._1, Deleted(r))
  }

  def /! (where: Location): Node = read(where) match {
    case Readen(mem: Node) => mem
    case _ => throw new IllegalArgumentException("Not a memory")
  }
}
