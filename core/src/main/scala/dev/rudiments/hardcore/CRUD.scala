package dev.rudiments.hardcore

import scala.collection.immutable.ListMap

sealed trait CRUD {}
object CRUD {}

sealed trait Cmd extends Command with CRUD
sealed trait Qry extends Query with CRUD
sealed trait Evt extends Event with CRUD
sealed trait Rep extends Report with CRUD
sealed trait Err extends Error with CRUD

case class Create(value: Any) extends Cmd
case class Read(where: Location) extends Qry
case class Update(old: Any, value: Any) extends Cmd
case class Delete(old: Any) extends Cmd

case class Created(value: Any) extends Evt
case class Readen(value: Any) extends Rep
case class Updated(old: Any, value: Any) extends Evt
case class Deleted(old: Any) extends Evt
case class Commit(events: (Location, Evt)*) extends Evt {
  def cud: Seq[(Location, Evt)] = events.foldLeft(Seq.empty[(Location, Evt)]) {
    case (m, (prefix, c: Commit)) => m ++ c.cud.map { case (l, evt) => prefix / l -> evt }
    case (m, p) => m :+ p //Created, Updated, Deleted
  }
  //TODO reject commit with intersecting locations inside?
}
case class Applied(commit: Commit) extends Evt

case class NotFound(id: Location) extends Rep
case class Conflict(incoming: Event, actual: Out) extends Err
case class NotSupported(in: In) extends Err
case class MultiError(errors: (Location, Out)*) extends Err
