package dev.rudiments.hardcore.eventstore

import akka.actor.Props
import dev.rudiments.hardcore.dsl._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}
import scala.language.postfixOps


class Task(f: PartialFunction[Command, Event])(implicit es: ActorEventStore)
  extends PartialFunction[Command, Event] {

  override def apply(command: Command): Event = {
    Await.result(future(command), 5 seconds)
  }

  def future(command: Command): Future[Event] = {
    if(!f.isDefinedAt(command)) throw new MatchError(command)

    val ref = es.system.actorOf(Props(new Action(f)))
    es.subscribe(ref)
    val promise = Promise[Event]
    ref ! WaitFor(command, promise)
    promise.future
  }

  override def isDefinedAt(x: Command): Boolean = f.isDefinedAt(x)
}

object Task {
  def apply(f: PartialFunction[Command, Event])(implicit es: ActorEventStore): Task =
    new Task(f)(es)
}

case class WaitFor(command: Command, promise: Promise[Event])