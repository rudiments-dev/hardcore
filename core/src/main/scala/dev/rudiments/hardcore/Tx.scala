package dev.rudiments.hardcore

import dev.rudiments.hardcore.Memory.{I, O}

import scala.collection.mutable

class Tx(ctx: Memory) {
  val total: mutable.Map[Location, mutable.Buffer[O]] = mutable.Map.empty
  val last: mutable.Map[Location, O] = mutable.Map.empty

  private def unsafeUpdateState(where: Location, what: O): O = {
    last.get(where) match {
      case Some(_) =>
        last += where -> what
        total(where) += what
        what
      case None =>
        last += where -> what
        total += where -> mutable.Buffer(what)
        what
    }
  }

  def remember(subj: Location, via: O): O = {
    (read(subj), via) match {
      case (NotExist, NotExist)                              => unsafeUpdateState(subj, NotExist)
      case (NotExist, c: Created)                            => unsafeUpdateState(subj, c)
      case (NotExist, r: Readen)                             => unsafeUpdateState(subj, r)
      case (Readen(found), Created(_))                       => AlreadyExist(found)
      case (r@Readen(r1), Readen(r2)) if r1 == r2            => r
      case (Readen(found), Updated(u2, data)) if found == u2 => unsafeUpdateState(subj, Updated(found, data))
      case (Readen(found), Deleted(d2))       if found == d2 => unsafeUpdateState(subj, Deleted(found))
      case (found, other)                                    => Conflict(found, other)
    }
  }

  def recall(subj: Location): Seq[O] = total.get(subj).map(_.toSeq).getOrElse(Seq.empty)

  def verify(): O = {
    val reduced = prepare()

    val errors = reduced.keys.map { k =>
      val v: O = (reduced(k), last(k)) match {
        case (c@Created(c1), Created(c2)) if c1 == c2 => c
        case (u@Updated(_, u1), Updated(_, u2)) if u1 == u2 => u
        case (d@Deleted(_), Deleted(_)) => d
        case (NotExist, NotExist) => NotExist
        case (that, other) => Conflict(that, other)
      }
      k -> v
    }.collect { case (l, e: Error) => (l, e) }.toMap

    if(errors.isEmpty) {
      Valid
    } else {
      MultiError(errors)
    }
  }

  def read(where: Location): O = last.get(where) match {
    case Some(Created(found)) => Readen(found)
    case Some(r: Readen) => r
    case Some(Updated(_, found)) => Readen(found)
    case Some(Deleted(_)) => NotExist
    case Some(NotExist) => NotExist
    case Some(_) => ???
    case None => unsafeUpdateState(where, ctx.read(where))
  }

  def ask(about: Location, in: I): O = {
    (read(about), in) match {
      case (NotExist, Create(data)) => Created(data)
      case (NotExist, _) => NotExist
      case (r: Readen, Read) => r
      case (Readen(found), Create(_)) => AlreadyExist(found)
      case (Readen(found), Update(data)) => Updated(found, data)
      case (Readen(found), Delete) => Deleted(found)
      case (found, other) => Conflict(found, other)
    }
  }

  def report(in: I): O = in match {
    case Verify => this.verify()
    case Prepare =>
      this.verify() match {
        case Valid => Prepared(Commit(prepare().collect { case (l, evt: Event) => (l, evt) }, null))
        case other => other
      }
    case _ => NotImplemented
  }

  def prepare(): Map[Location, O] =
    total.view.mapValues(_.toSeq.reduce(Tx.reducer(_, _))).toMap

  def >? : O = this.report(Verify)
  def >> : O = this.report(Prepare)

  def ? (where: Location): O = this.ask(where, Read)
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
}

object Tx {
  val reducer: PartialFunction[(O, O), O] = {
    case (   NotExist,      c: Created)                    => c
    case (   NotExist,      r: Readen)                     => Conflict(NotExist, r)
    case (   NotExist,      u: Updated)                    => Conflict(NotExist, u)
    case (   NotExist,      d: Deleted)                    => Conflict(NotExist, d)

    case (   Created(c1),      Created(_))                 => AlreadyExist(c1)
    case ( c@Created(c1),      Readen(r2))     if c1 == r2 => c
    case (   Created(c1),    Updated(u1, u2)) if c1 == u1 => Created(u2)
    case (   Created(c1),    d@Deleted(d2))    if c1 == d2 => NotExist

    case (   Readen(r1),       Created(_))                 => AlreadyExist(r1)
    case ( r@Readen(r1),       Readen(r2))     if r1 == r2 => r
    case (   Readen(r1),     u@Updated(u2, _)) if r1 == u2 => u
    case (   Readen(r1),     d@Deleted(d2))    if r1 == d2 => d

    case (   Updated(_, u1),   Created(_))                 => AlreadyExist(u1)
    case ( u@Updated(_, u1),   Readen(r2))     if u1 == r2 => u
    case (   Updated(u11, u12), Updated(u21, u22)) if u12 == u21 => Updated(u11, u22)
    case (   Updated(u1, u2), Deleted(d2))    if u2 == d2  => Deleted(u1)

    case (   Deleted(_),    c: Created)                    => c
    case ( d:Deleted,       r: Readen)                     => AlreadyNotExist(d, r)
    case ( d:Deleted,       u: Updated)                    => AlreadyNotExist(d, u)
    case (d1:Deleted,      d2: Deleted)                    => AlreadyNotExist(d1, d2)
    case (that, other) /* unfitting updates */             => Conflict(that, other)
  }
}