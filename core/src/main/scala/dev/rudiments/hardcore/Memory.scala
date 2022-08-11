package dev.rudiments.hardcore

import dev.rudiments.hardcore.CRUD.{Evt, I, O}
import dev.rudiments.hardcore.Memory.commits
import dev.rudiments.hardcore.Predicate.All

case class Memory(
  node: Node = Node.empty
) extends AgentCrud {

  Initial.init(this)

  private def unsafeUpdateState(where: Location, what: Evt): Evt = node.remember(where, what) match {
    case evt: Evt => evt
    case other =>
      throw new IllegalArgumentException(s"whut? $other")
  }

  override def read(where: Location): O = node.read(where)

  override def remember(where: Location, via: O): O = {
    (this ? where, via) match {
      case (NotExist, c: Created)                  => unsafeUpdateState(where, c)
      case (NotFound(_), c: Created)               => unsafeUpdateState(where, c)
      case (Readen(_: Node), c@Created(_: Data))   => unsafeUpdateState(where, c)
      case (Readen(n: Node), u@Updated(old, _: Data)) if n.self == old => unsafeUpdateState(where, u)
      case (Readen(r), Created(_))                 =>
        AlreadyExist(r)
      case (Readen(r), Updated(u, data)) if r == u => unsafeUpdateState(where, Updated(r, data))
      case (Readen(r), Deleted(d)) if r == d       => unsafeUpdateState(where, Deleted(r))
      case (found, other)                          => Conflict(found, other)
    }
  }

  override def find(where: Location, p: Predicate): O = node.find(where, p)

  def commit(c: Commit): O = {
    val outputs = c.crud
      .map { case (id, evt: Evt) =>
        id -> remember(id, evt)
      }

    val crudErrors = outputs.collect {
        case (id, err: Error) => (id, err)
        case (id, NotExist) => (id, NotExist)
      }

    val p = commits / ID(c.hashCode().toString)
    val errors = this += p -> c match {
      case _: Created => crudErrors
      case m: MultiError => crudErrors + (p -> m)
      case e: Error => crudErrors + (p -> e)
      case other => crudErrors + (p -> Conflict(c, other))
    }

    if(errors.isEmpty) {
      Committed(c)
    } else {
      MultiError(errors) //TODO rollback?
    }
  }

  def execute(in: I): O = in match {
    case c: Commit => commit(c)
    case Find(All) => Found(All, node.find())
    case _ =>
      NotImplemented
  }

  def << (in: I) : O = this.execute(in)
}

object Memory {
  val commits: ID = ID("commits")

  val reducer: PartialFunction[(Evt, Evt), O] = {
    case (   Created(c1),      Created(_))                 => AlreadyExist(c1)
    case (   Created(c1),    u@Updated(u2, _)) if c1 == u2 => u
    case (   Created(c1),    d@Deleted(d2))    if c1 == d2 => d

    case (   Updated(_, u1),   Created(_))                 => AlreadyExist(u1)
    case (   Updated(_, u1), u@Updated(u2, _)) if u1 == u2 => u
    case (   Updated(_, u1), d@Deleted(d2))    if u1 == d2 => d

    case (   Deleted(_),     c@Created(_))                 => c
    case (that, other) /* unfitting updates */             => Conflict(that, other)
  }
}
