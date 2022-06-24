package dev.rudiments.hardcore

import scala.collection.mutable

class Tx(
  ctx: Memory
) {
  val total: mutable.Map[Location, mutable.Seq[Out]] = mutable.Map.empty
  val last: mutable.Map[Location, Out] = mutable.Map.empty

  private def unsafeUpdateState(where: Location, what: Out): Out = {
    last.get(where) match {
      case Some(_) =>
        last += where -> what
        total(where).+:(what)
        what
      case None =>
        last += where -> what
        total += where -> mutable.Seq(what)
        what
    }
  }

  def remember(subj: Location, via: Out): Out = {
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

  def recall(subj: Location): Seq[Out] = total.get(subj).map(_.toSeq).getOrElse(Seq.empty)

  def read(where: Location): Out = last.get(where) match {
    case Some(Created(found)) => Readen(found)
    case Some(Updated(_, found)) => Readen(found)
    case Some(Deleted(_)) => NotExist
    case Some(NotExist) => NotExist
    case None =>
      ctx.read(where) match {
        case r: Readen => unsafeUpdateState(where, r)
        case NotExist => unsafeUpdateState(where, NotExist)
      }
  }

  def ask(about: Location, in: In): Out = {
    (read(about), in) match {
      case (NotExist, Create(data)) => Created(data)
      case (NotExist, _) => NotExist
      case (r: Readen, Read) => r
      case (Readen(found), Create(_)) => AlreadyExist(found)
      case (Readen(found), Update(data)) => Updated(found, data)
      case (Readen(found), Delete) => Deleted(found)
    }
  }

  val reducer: PartialFunction[(Out, Out), Out] = {
    case (   NotExist,      c: Created)                    => c
    case (   NotExist,      r: Readen)                     => Conflict(NotExist, r)
    case (   NotExist,      u: Updated)                    => Conflict(NotExist, u)
    case (   NotExist,      d: Deleted)                    => Conflict(NotExist, d)

    case (   Created(c1),      Created(_))                 => AlreadyExist(c1)
    case ( c@Created(c1),      Readen(r2))     if c1 == r2 => c
    case (   Created(c1),    u@Updated(u2, _)) if c1 == u2 => u
    case (   Created(c1),    d@Deleted(d2))    if c1 == d2 => d

    case (   Readen(r1),       Created(_))                 => AlreadyExist(r1)
    case ( r@Readen(r1),       Readen(r2))     if r1 == r2 => r
    case (   Readen(r1),     u@Updated(u2, _)) if r1 == u2 => u
    case (   Readen(r1),     d@Deleted(d2))    if r1 == d2 => d

    case (   Updated(_, u1),   Created(_))                 => AlreadyExist(u1)
    case ( u@Updated(_, u1),   Readen(r2))     if u1 == r2 => u
    case (   Updated(_, u1), u@Updated(u2, _)) if u1 == u2 => u
    case (   Updated(_, u1), d@Deleted(d2))    if u1 == d2 => d

    case (   Deleted(_),    c: Created)                    => c
    case ( d:Deleted,       r: Readen)                     => AlreadyNotExist(d, r)
    case ( d:Deleted,       u: Updated)                    => AlreadyNotExist(d, u)
    case (d1:Deleted,      d2: Deleted)                    => AlreadyNotExist(d1, d2)
    case (that, other) /* unfitting updates */             => Conflict(that, other)
  }
}

