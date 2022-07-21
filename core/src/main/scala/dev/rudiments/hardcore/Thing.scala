package dev.rudiments.hardcore

import dev.rudiments.hardcore.Predicate.All

sealed trait Thing {}

sealed trait Relation extends Thing {} // (obj, rel, subj) => obj -> (rel, subj), assuming rel or subj can be data and obj is always 'self'
// usual relation: id -> (id, id)
// data inside memory is a relation: id -> (predicate, any)
// memory spec: id -> (Can, <Create, Read, Update, Delete>), id -> (Store, <Created, Updated, Deleted>), Q: CRUD events are also Relations?
case object Can extends Relation {}
case object Holds extends Relation {}

trait Agent extends Thing {
  def read(where: Location): CRUD.O
  def ?(where: Location): CRUD.O = read(where)

  def find(where: Location, p: Predicate = All): CRUD.O
  def ?? (where: Location): CRUD.O = find(where, All)
}

final case class Link(where: Location, what: Predicate) extends Predicate
final case class Data(what: Predicate, data: Any) extends Thing
object Data {
  val empty = Data(Nothing, Nothing)
}

sealed trait Predicate extends Thing {}
object Predicate {
  case object All extends Predicate
  case object Anything extends Predicate
}
final case class Type(fields: Field*) extends Predicate
final case class Field(name: String, of: Predicate) //TODO snapshot & restore for Memory[Text, Field] -> Type -> Memory[Text, Field]

final case class Enlist(item: Predicate) extends Predicate {}
final case class Index(of: Predicate, over: Predicate) extends Predicate {}
final case class AnyOf(p: Predicate*) extends Predicate {} // Sum-Type

sealed trait Plain extends Predicate {}
final case class Text(maxSize: Int) extends Plain
final case class Number(from: AnyVal, upTo: AnyVal) extends Plain //TODO replace with more strict version
case object Bool extends Plain {} // funny thing - in scala we can't extend object, so, or 'AnyBool' under trait, or no True and False under Bool object
case object Binary extends Plain {} // Array[Byte]

sealed trait Abstraction extends Thing {}
case object Nothing extends Predicate {}

trait Message extends Thing {} //TODO separate CRUD+ from Message
trait In extends Message {}
trait Out extends Message {}
trait Command extends In {}
trait Event extends Out {}
trait Query extends In {}
trait Report extends Out {}
trait Error extends Out {}