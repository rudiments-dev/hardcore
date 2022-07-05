package dev.rudiments.hardcore

sealed trait CRUD {}

final case class Create(what: Data) extends Command with CRUD
case object Read extends Query with CRUD
final case class Update(what: Data) extends Command with CRUD
case object Delete extends Command with CRUD
final case class Find(p: Predicate) extends Query with CRUD

final case class Created(data: Data) extends Event with CRUD
final case class Readen(data: Data) extends Report with CRUD
final case class Updated(old: Data, data: Data) extends Event with CRUD
final case class Deleted(old: Data) extends Event with CRUD
final case class Found(p: Predicate, values: Map[Location, Data]) extends Report with CRUD

case object NotExist extends Report with CRUD
final case class AlreadyExist(data: Data) extends Error with CRUD
final case class AlreadyNotExist(that: Message, other: Message) extends Error with CRUD
final case class Conflict(that: Message, other: Message) extends Error with CRUD

case object Prepare extends Query with CRUD
final case class Prepared(commit: Commit) extends Report with CRUD
final case class Commit(
  crud: Map[Location, Event],
  basedOn: Commit,
  extra: Seq[(Command, Event)] = Seq.empty // for future use
) extends Command with CRUD
object Commit {
  val beginningOfTime: Commit = Commit(Map.empty, null, Seq.empty)
}

final case class Committed(commit: Commit) extends Event with CRUD

case object Verify extends Query with CRUD
case object Valid extends Report with CRUD
final case class MultiError(errors: Map[Location, Error]) extends Error with CRUD
case object Inconsistent extends Error with CRUD
case object NotImplemented extends Error with CRUD