package dev.rudiments.hardcore


sealed trait CRUD {}
object CRUD {
  type Evt = Event with CRUD
  type Cmd = Command with CRUD
  type O = Out with CRUD
  type I = In with CRUD
}

final case class  Create(what: Thing) extends Command with CRUD
case object       Read extends Query with CRUD
final case class  Update(what: Thing) extends Command with CRUD
case object       Delete extends Command with CRUD
final case class  Find(p: Predicate) extends Query with CRUD

final case class Created(data: Thing) extends Event with CRUD
final case class Readen(data: Thing) extends Report with CRUD
final case class Updated(old: Thing, data: Thing) extends Event with CRUD
final case class Deleted(old: Thing) extends Event with CRUD
final case class Found(p: Predicate, values: Map[Location, Thing]) extends Report with CRUD
case object NotExist extends Report with CRUD
case class NotFound(missing: Location) extends Report with CRUD

final case class AlreadyExist(data: Thing) extends Error with CRUD
final case class Conflict(that: Message, other: Message) extends Error with CRUD

case object Prepare extends Query with CRUD
final case class Prepared(commit: Commit) extends Report with CRUD
final case class Commit(
  crud: Map[Location, CRUD.Evt],
  extra: Seq[(Command, Event)] = Seq.empty // for future use
) extends Command with CRUD
object Commit {
  val beginningOfTime: Commit = Commit(Map.empty, Seq.empty)
}

final case class Committed(commit: Commit) extends Event with CRUD

case object Verify extends Query with CRUD
case object Valid extends Report with CRUD
final case class MultiError(errors: Map[Location, Out]) extends Error with CRUD
case object NotImplemented extends Error with CRUD
