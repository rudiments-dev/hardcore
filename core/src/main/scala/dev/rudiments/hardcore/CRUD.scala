package dev.rudiments.hardcore

import scala.collection.immutable.ListMap

sealed trait CRUD {}
object CRUD {}

case class Create(value: Product) extends Command with CRUD
case class Read(where: Location) extends Query with CRUD
case class Update(old: Product, value: Product) extends Command with CRUD
case class Delete(old: Product) extends Command with CRUD

case class Created(value: Product) extends Event with CRUD
case class Readen(value: Product) extends Report with CRUD
case class Updated(old: Product, value: Product) extends Event with CRUD
case class Deleted(old: Product) extends Event with CRUD
case class Commit(events: (Location, Event with CRUD)*) extends Event with CRUD {
  private val indexed = events.toMap
  if(indexed.size != events.size) {
    throw new IllegalArgumentException("Only unique locations allowed")
  }

  def flatten: Seq[(Location, Event with CRUD)] = events.foldLeft(Seq.empty[(Location, Event with CRUD)]) {
    case (m, (prefix, c: Commit)) => m ++ c.flatten.map { case (l, evt) => prefix / l -> evt }
    case (m, p) => m :+ p //Created, Updated, Deleted
  } //TODO reject commit with intersecting locations inside?

  override def toString: String = events.map { (l, evt) => evt match
    case Created(v) => s"$l +$v"
    case Updated(v1, v2) => s"$l *$v1|->$v2"
    case Deleted(v) => s"$l -$v"
    case c: Commit => s"$l: Commit($c)"
    case other => s"$l -> {$other}"
  }.mkString(";")
}

case class NotFound(id: Location) extends Report with CRUD
case object Identical extends Report with CRUD
case class Conflict(incoming: Event, actual: Out) extends Error with CRUD
case class NotSupported(in: In) extends Error with CRUD
case class MultiError(errors: (Location, Out)*) extends Error with CRUD
case class InternalError(t: Throwable) extends Error with CRUD
