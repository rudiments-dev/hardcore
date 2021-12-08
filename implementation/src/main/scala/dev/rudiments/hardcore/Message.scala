package dev.rudiments.hardcore

trait Message {}
trait In extends Message {}
trait Command extends In {}
trait Query extends In {}
trait Out extends Message {}
trait Event extends Out {}
trait Report extends Out {}
trait Error extends Out {}

sealed trait CRUD {}
case class Create(id: ID, data: Data) extends Command with CRUD
case class Read(id: ID) extends Query with CRUD
case class Update(id: ID, data: Data) extends Command with CRUD
case class Delete(id: ID) extends Command with CRUD

case class Created(id: ID, data: Data) extends Event with CRUD
case class Readen(id: ID, data: Data) extends Report with CRUD
case class Updated(id: ID, oldData: Data, newData: Data) extends Event with CRUD
case class Deleted(id: ID, data: Data) extends Event with CRUD

case class NotFound(id: ID) extends Error with CRUD
case class AlreadyExist(id: ID, data: Data) extends Error with CRUD
case class Conflict(id: ID) extends Error with CRUD


case class Find(p: Predicate = All) extends Query

case class Found(p: Predicate, data: Map[ID, Data]) extends Report