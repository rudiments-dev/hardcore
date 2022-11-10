package dev.rudiments.hardcore

import dev.rudiments.hardcore.CRUD.{I, O}

import scala.collection.mutable

class Tx(ctx: Agent) extends AgentCrud {
  val total: mutable.Map[Location, mutable.Buffer[O]] = mutable.Map.empty
  val last: mutable.Map[Location, O] = mutable.Map.empty

  override def read(where: Location): O = last.get(where) match {
    case Some(Created(found)) => Readen(found)
    case Some(r: Readen) => r
    case Some(Updated(_, found)) => Readen(found)
    case Some(Deleted(_)) => NotExist
    case Some(NotExist) => NotExist
    case Some(n: NotFound) => n
    case Some(other) => throw new IllegalArgumentException(s"don't know $other")
    case None => unsafeUpdateState(where, ctx ? where)
  }

  override def remember(subj: Location, via: O): O = {
    (read(subj), via) match {
      case (NotExist, NotExist)                              => unsafeUpdateState(subj, NotExist)
      case (_: NotFound, NotExist)                           => unsafeUpdateState(subj, NotExist)
      case (NotExist, c: Created)                            => unsafeUpdateState(subj, c)
      case (NotFound(_), c: Created)                         => unsafeUpdateState(subj, c)
      case (NotExist, r: Readen)                             => unsafeUpdateState(subj, r)
      case (NotFound(_), r: Readen)                          => unsafeUpdateState(subj, r)
      case (Readen(found), Created(_))                       => AlreadyExist(found)
      case (r@Readen(r1), Readen(r2)) if r1 == r2            => r
      case (Readen(found), Updated(u2, data)) if found == u2 => unsafeUpdateState(subj, Updated(found, data))
      case (Readen(mem: Node), Updated(u, data)) if mem.self == u => unsafeUpdateState(subj, Updated(u, data))
      case (Readen(found), Deleted(d2))       if found == d2 => unsafeUpdateState(subj, Deleted(found))
      case (found, other)                                    =>
        Conflict(found, other)
    }
  }

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

  def verify(): O = {
    val reduced = prepare()

    val errors = reduced.keys.map { k =>
      val v: O = (reduced(k), last(k)) match {
        case (c@Created(c1), Created(c2)) if c1 == c2 => c
        case (u@Updated(_, u1), Updated(_, u2)) if u1 == u2 => u
        case (d@Deleted(_), Deleted(_)) => d
        case (NotExist, NotExist) => NotExist
        case (nf@NotFound(nf1), NotFound(nf2)) if nf1 == nf2 => nf
        case (r@Readen(r1), Readen(r2)) if r1 == r2 => r
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

  override def report(q: Query): O = q match {
    case Verify => this.verify()
    case Prepare =>
      this.verify() match {
        case Valid => Prepared(Commit(prepare().collect { case (l, evt: Event) => (l, evt) }))
        case other => other
      }
    case _ =>
      NotImplemented
  }

  def prepare(): Map[Location, O] =
    total.view.mapValues(_.toSeq.reduce(Tx.reducer(_, _))).toMap

  def >? : O = this.report(Verify)
  def >> : O = this.report(Prepare)
}

object Tx {
  val reducer: PartialFunction[(O, O), O] = {
    case (   NotExist,      c: Created)                    => c
    case (   NotExist,      r: Readen)                     => Conflict(NotExist, r)
    case (   NotExist,      u: Updated)                    => Conflict(NotExist, u)
    case (   NotExist,      d: Deleted)                    => Conflict(NotExist, d)

    case (n: NotFound,      c: Created)                    => c
    case (n: NotFound,      r: Readen)                     => Conflict(n, r)
    case (n: NotFound,      u: Updated)                    => Conflict(n, u)
    case (n: NotFound,      d: Deleted)                    => Conflict(n, d)

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
    case ( d:Deleted,       r: Readen)                     => Conflict(d, r)
    case ( d:Deleted,       u: Updated)                    => Conflict(d, u)
    case (d1:Deleted,      d2: Deleted)                    => Conflict(d1, d2)
    case (that, other) /* unfitting updates */             => Conflict(that, other)
  }
}