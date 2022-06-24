package dev.rudiments.hardcore

import scala.collection.mutable

class Memory {
  val total: mutable.Map[Location, mutable.Seq[Event]] = mutable.Map.empty
  val last: mutable.Map[Location, Event] = mutable.Map.empty
  val commits: mutable.Buffer[Commit] = mutable.Buffer.empty

  private def unsafeUpdateState(where: Location, what: Event): Event = {
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

  def remember(subj: Location, via: Event): Out = {
    (read(subj), via) match {
      case (NotExist, c: Created)                  => unsafeUpdateState(subj, c)
      case (Readen(r), Created(_))                 => AlreadyExist(r)
      case (Readen(r), Updated(u, data)) if r == u => unsafeUpdateState(subj, Updated(r, data))
      case (Readen(r), Deleted(d)) if r == d       => unsafeUpdateState(subj, Deleted(r))
      case (found, other)                          => Conflict(found, other)
    }
  }

  def recall(subj: Location): Seq[Event] = total.get(subj).map(_.toSeq).getOrElse(Seq.empty)

  def read(where: Location): Out = last.get(where) match {
    case Some(Created(found)) => Readen(found)
    case Some(Updated(_, found)) => Readen(found)
    case Some(Deleted(_)) => NotExist
    case Some(Committed(_)) => ???
    case None => NotExist
  }

  def ask(about: Location, in: In): Out = {
    (read(about), in) match {
      case (NotExist, Create(data)) => Created(data)
      case (NotExist, _) => NotExist
      case (r: Readen, Read) => r
      case (Readen(found), Create(_)) => AlreadyExist(found)
      case (Readen(found), Update(data)) => Updated(found, data)
      case (Readen(found), Delete) => Deleted(found)
      case (other, another) => Conflict(other, another)
    }
  }

  def execute(in: In): Out = in match {
    case c: Commit =>
      val errors = c.crud
        .map { case (id, evt) => id -> remember(id, evt) }
        .collect { case (id, err: Error) => (id, err) }

      if(errors.isEmpty) {
        commits += c
        Committed(c)
      } else {
        MultiError(errors) //TODO rollback?
      }

    case Find(All) =>
      Found(All, last.toMap.collect {
        case (id, Created(data)) => id -> data
        case (id, Updated(_, data)) => id -> data
      })

    case _ => NotImplemented
  }

  val reducer: PartialFunction[(Event, Event), Out] = {
    case (   Created(c1),      Created(_))                 => AlreadyExist(c1)
    case (   Created(c1),    u@Updated(u2, _)) if c1 == u2 => u
    case (   Created(c1),    d@Deleted(d2))    if c1 == d2 => d

    case (   Updated(_, u1),   Created(_))                 => AlreadyExist(u1)
    case (   Updated(_, u1), u@Updated(u2, _)) if u1 == u2 => u
    case (   Updated(_, u1), d@Deleted(d2))    if u1 == d2 => d

    case (   Deleted(_),     c@Created(_))                 => c
    case ( d@Deleted(_),     u@Updated(_, _))              => AlreadyNotExist(d, u)
    case (d1@Deleted(_),    d2@Deleted(_))                 => AlreadyNotExist(d1, d2)
    case (that, other) /* unfitting updates */             => Conflict(that, other)
  }

  def << (in: In) : Out = this.execute(in)
}

object Memory {
  implicit class MemoryOps(where: Location)(implicit memory: Memory) {
    def ? : Out = memory.ask(where, Read)
    def +(data: Data) : Out = memory.ask(where, Create(data))
    def *(data: Data) : Out = memory.ask(where, Update(data))
    def - : Out = memory.ask(where, Delete)

    def +=(data: Data): Out = memory.remember(where, Created(data))
    def *=(data: Data): Out = {
      memory.read(where) match {
        case Readen(found) => memory.remember(where, Updated(found, data))
        case NotExist => NotExist
        case _ => ???
      }
    }
    def -= : Out = memory.read(where) match {
      case Readen(found) => memory.remember(where, Deleted(found))
      case NotExist => NotExist
      case _ => ???
    }

    def << (cmd: Command): Out = memory.ask(where, cmd) match {
      case evt: Event => memory.remember(where, evt)
      case other => other
    }
  }
}