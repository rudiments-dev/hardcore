package dev.rudiments.hardcore

import dev.rudiments.hardcore.CRUD.{Evt, O}
import dev.rudiments.hardcore.Memory.commits

case class Memory(
  node: Node = Node.empty
) extends AgentCrud {

  Initial.init(this)

  private def nodeEvent(where: Location, what: Evt): Evt = node.remember(where, what) match {
    case evt: Evt => evt
    case other =>
      throw new IllegalArgumentException(s"whut? $other")
  }

  override def read(where: Location): O = node.read(where)

  override def remember(where: Location, via: O): O = {
    (this ? where, via) match {
      case (NotExist, c: Created)                  => nodeEvent(where, c)
      case (NotFound(_), c: Created)               => nodeEvent(where, c)
      case (Readen(_: Node), c@Created(_: Data))   => nodeEvent(where, c)
      case (Readen(n: Node), u@Updated(old, _: Data)) if n.self == old => nodeEvent(where, u)
      case (Readen(r), Created(_))                 =>
        AlreadyExist(r)
      case (Readen(r), Updated(u, data)) if r == u => nodeEvent(where, Updated(r, data))
      case (Readen(r), Deleted(d)) if r == d       => nodeEvent(where, Deleted(r))
      case (NotExist, Committed(cmt))              =>
        val n = Node.empty
        node += where -> n
        commit(where, n, cmt) //assuming commit contains create for root
      case (NotFound(miss), Committed(cmt))        =>
        val n = Node.empty
        node += where -> n
        commit(where, n, cmt)
      case (Readen(n: Node), Committed(cmt))       => commit(where, n, cmt)
      case (found, other)                          => Conflict(found, other)
    }
  }

  override def report(q: Query): O = node.report(q)

  private def commit(where: Location, n: Node, cmt: Commit): O = {
    val remember = Commit(cmt.crud.map { case (l, evt) => where / l -> evt })
    node += commits / remember.hashCode().toString -> remember match {
      case _: Created => n.commit(cmt)
      case err: Error => err
    }
  }

  def << (c: Commit) : O = this.remember(Root, Committed(c))
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
