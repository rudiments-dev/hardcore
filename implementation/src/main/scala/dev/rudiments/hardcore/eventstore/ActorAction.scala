package dev.rudiments.hardcore.eventstore

import akka.actor.{Actor, Props}
import com.typesafe.scalalogging.StrictLogging
import dev.rudiments.hardcore.dsl.{Action, Command, Event, Handler}

import scala.concurrent.Promise

class ActorAction(val f: Handler, val command: Command, val promise: Promise[Event])(implicit es: ActorEventStore)
  extends Actor with StrictLogging with Action {

  override def receive: Receive = {
    case ActorAction.GoOn(c: Command) if command == c =>
      val result = f(command)
      context.system.eventStream.unsubscribe(self)
      es.ref ! (command, result)
      promise.success(result)

    case ActorAction.InProgress(c: Command) if command == c =>
      logger.debug("Command {} execution in progress", command)

    case ActorAction.Done(c, event) if command == c =>
      promise.success(event)

    case (c: Command, events: Seq[Event] @unchecked) if command == c =>
      promise.success(events.head)
  }
}

object ActorAction {
  case class GoOn(command: Command)
  case class Done(command: Command, event: Event)
  case class InProgress(command: Command)
  def props(f: Handler, command: Command, promise: Promise[Event])(implicit es: ActorEventStore): Props =
    Props(new ActorAction(f, command, promise))
}