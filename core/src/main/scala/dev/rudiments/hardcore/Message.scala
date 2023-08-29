package dev.rudiments.hardcore

sealed trait Message extends Product
sealed trait In extends Message
sealed trait Out extends Message

trait Command extends In
trait Query extends In
trait Event extends Out
trait Report extends Out
trait Error extends Report

case class Create(value: Any) extends Command
case class Read(where: Location) extends Query
case class Update(old: Any, value: Any) extends Command
case class Delete(old: Any) extends Command

case class Created(value: Any) extends Event
case class Readen(value: Any) extends Report
case class Updated(old: Any, value: Any) extends Event
case class Deleted(old: Any) extends Event

case class NotFound(where: Location) extends Report
case object Identical extends Report
case class Conflict(in: Event, actual: Out) extends Error
case class NotSupported(m: Message) extends Error
case class InternalError(t: Throwable) extends Error

type Response = Event | Report