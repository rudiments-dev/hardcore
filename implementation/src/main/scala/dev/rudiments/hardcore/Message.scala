package dev.rudiments.hardcore

trait Message {}
trait In extends Message {}
trait Command extends In {}
trait Query extends In {}
trait Out extends Message {}
trait Event extends Out {}
trait Report extends Out {}
trait Error extends Out {}

sealed abstract class CRUD(val id: ID) {}
case class Create(override val id: ID, data: Data) extends CRUD(id) with Command
case class Read(override val id: ID) extends CRUD(id) with Query
case class Update(override val id: ID, data: Data) extends CRUD(id) with Command
case class Delete(override val id: ID) extends CRUD(id) with Command

case class Created(override val id: ID, data: Data) extends CRUD(id) with Event
case class Readen(override val id: ID, data: Data) extends CRUD(id) with Report
case class Updated(override val id: ID, oldData: Data, newData: Data) extends CRUD(id) with Event
case class Deleted(override val id: ID, data: Data) extends CRUD(id) with Event

case class NotFound(override val id: ID) extends CRUD(id) with Error
case class AlreadyExist(override val id: ID, data: Data) extends CRUD(id) with Error
case class Conflict(override val id: ID) extends CRUD(id) with Error


case class Find(p: Predicate = All) extends Query
case class Found(p: Predicate, data: Map[ID, Data]) extends Report

//TODO naming
case class Apply(log: Seq[In]) extends Command
case class Commit(events: Seq[(In, Out)]) extends Event