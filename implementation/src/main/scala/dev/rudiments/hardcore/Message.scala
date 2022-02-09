package dev.rudiments.hardcore

trait Message extends ADT {}
trait In extends Message {} // input
trait Command extends In {} // mutator input
trait Query extends In {} // read-only input

trait Out extends Message {} // output
trait Event extends Out {} // mutation happened, state transferred
trait Report extends Out {} // response to query, no mutation
trait Error extends Out {} // failed response, mutation possible under circumstances

sealed abstract class CRUD(val id: ID) {}
case class Create(override val id: ID, data: Thing) extends CRUD(id) with Command
case class Read(override val id: ID) extends CRUD(id) with Query
case class Update(override val id: ID, data: Thing) extends CRUD(id) with Command
case class Delete(override val id: ID) extends CRUD(id) with Command

case class Created(override val id: ID, data: Thing) extends CRUD(id) with Event
case class Readen(override val id: ID, data: Thing) extends CRUD(id) with Report
case class Updated(override val id: ID, oldData: Thing, newData: Thing) extends CRUD(id) with Event
case class Deleted(override val id: ID, data: Thing) extends CRUD(id) with Event

case class NotFound(override val id: ID) extends CRUD(id) with Error
case class AlreadyExist(override val id: ID, data: Thing) extends CRUD(id) with Error
case class Conflict(override val id: ID) extends CRUD(id) with Error


case class Find(p: Predicate = All) extends Query
case class Found(p: Predicate, data: Map[ID, Thing]) extends Report

case class AddRelation(id: ID, path: Path) extends Command
case class RelationAdded(id: ID, path: Path) extends Event
case class AlreadyRelated(id: ID, path: Path) extends Error

case class NotImplemented(in: In) extends Error
