package dev.rudiments.hardcore.dsl

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}

trait Task extends Handler{
  val es: EventStore
  val f: Handler

  override def apply(command: Command): Event = {
    Await.result(future(command), Task.defaultTimeout)
  }

  def future(command: Command): Future[Event]

  override def isDefinedAt(x: Command): Boolean = f.isDefinedAt(x)

  def orElse(that: Task): Task = new OrElseTask(this, that)
}

object Task {
  val defaultTimeout: Duration = Duration("5 seconds")
}

class OrElseTask(t1: Task, t2: Task) extends Task {
  override val es: EventStore = t1.es
  val f: Handler = t1.f.orElse(t2.f)

  override def future(command: Command): Future[Event] = {
    if(t1.isDefinedAt(command)) t1.future(command)
    else if(t2.isDefinedAt(command)) t2.future(command)
    else throw new MatchError(command)
  }
}

trait Action {
  val f: Handler
  val command: Command
  val promise: Promise[Event]
}

trait EventStore {
  def state(): Future[Seq[Event]]
//TODO how to pass implicit params?
//  def willDo(command: Command): Unit
//  def done(command: Command, event: Event): Unit
}
