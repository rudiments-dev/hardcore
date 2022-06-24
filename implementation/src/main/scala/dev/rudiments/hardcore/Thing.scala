package dev.rudiments.hardcore

sealed trait Thing {}

sealed trait Relation extends Thing {} // (obj, rel, subj) => obj -> (rel, subj), assuming rel or subj can be data and obj is always 'self'
// usual relation: id -> (id, id)
// data inside memory is a relation: id -> (predicate, any)
// memory spec: id -> (Can, <Create, Read, Update, Delete>), id -> (Store, <Created, Updated, Deleted>), Q: CRUD events are also Relations?
case object Can extends Relation {}
case object Holds extends Relation {}

case object All extends Predicate

final case class Link(where: Location, what: Predicate) extends Thing
final case class Data(what: Predicate, data: Any) extends Thing

sealed trait Predicate extends Thing {}
final case class Type(fields: Field*) extends Predicate
final case class Field(name: String, of: Predicate) //TODO snapshot & restore for Memory[Text, Field] -> Type -> Memory[Text, Field]

sealed trait Plain extends Predicate {}
final case class Text(maxSize: Int) extends Plain
final case class Number(from: AnyVal, upTo: AnyVal) extends Plain //TODO replace with more strict version
case object Bool extends Plain {} // funny thing - in scala we can't extend object, so, or 'AnyBool' under trait, or no True and False under Bool object

sealed trait Message extends Thing {} //TODO separate CRUD+ from Message
sealed trait In extends Message {}
sealed trait Out extends Message {}
sealed trait Command extends In {}
sealed trait Event extends Out {}
sealed trait Query extends In {}
sealed trait Report extends Out {}
sealed trait Error extends Out {}

final case class Create(what: Data) extends Command
case object Read extends Query
final case class Update(what: Data) extends Command
case object Delete extends Command
final case class Find(p: Predicate) extends Query

final case class Created(data: Data) extends Event
final case class Readen(data: Data) extends Report
final case class Updated(old: Data, data: Data) extends Event
final case class Deleted(old: Data) extends Event
final case class Found(p: Predicate, values: Map[Location, Data]) extends Report

case object NotExist extends Report
final case class AlreadyExist(data: Data) extends Error
final case class AlreadyNotExist(that: Message, other: Message) extends Error
final case class Conflict(that: Message, other: Message) extends Error

case object Prepare extends Query
final case class Prepared(commit: Commit) extends Report
final case class Commit(
  crud: Map[Location, Event],
  basedOn: Commit,
  extra: Seq[(Command, Event)] = Seq.empty // for future use
) extends Command
object Commit {
  val beginningOfTime: Commit = Commit(Map.empty, null, Seq.empty)
}

final case class Committed(commit: Commit) extends Event

case object Verify extends Query
case object Valid extends Report
final case class MultiError(errors: Map[Location, Error]) extends Error
case object Inconsistent extends Error
case object NotImplemented extends Error