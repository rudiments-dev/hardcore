package dev.rudiments.hardcore

trait Message {}
trait In extends Message {}
trait Out extends Message {}
trait Error extends Out {}

sealed trait CRUD {}
case class Create(id: ID, data: Data) extends In with CRUD
case class Read(id: ID) extends In with CRUD
case class Update(id: ID, data: Data) extends In with CRUD
case class Delete(id: ID) extends In with CRUD

case class Created(id: ID, data: Data) extends Out with CRUD
case class Readen(id: ID, data: Data) extends Out with CRUD
case class Updated(id: ID, oldData: Data, newData: Data) extends Out with CRUD
case class Deleted(id: ID, data: Data) extends Out with CRUD

case class NotFound(id: ID) extends Error with CRUD
case class AlreadyExist(id: ID, data: Data) extends Error with CRUD


case class Apply(data: Map[ID, CRUD]) extends In
case class Commit(data: Map[ID, Data]) extends Out
