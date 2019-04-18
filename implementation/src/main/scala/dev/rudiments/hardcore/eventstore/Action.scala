package dev.rudiments.hardcore.eventstore

import akka.actor.Actor
import com.typesafe.scalalogging.StrictLogging
import dev.rudiments.hardcore.dsl.{Command, Done, Event, InProgress}

import scala.concurrent.Promise

class Action(f: Command => Event)(implicit es: ActorEventStore)
  extends Actor with StrictLogging {

  var p: Promise[Event] = _
  var c: Command = _
  override def receive: Receive = {
    case WaitFor(command, promise) =>
      es.ref ! command
      p = promise
      c = command

    case GoOn(command: Command) if command == c =>
      val result = f(command)
      context.system.eventStream.unsubscribe(self)
      es.ref ! (command, result)
      p.success(result)

    case (command: Command, InProgress) if command == c =>
      logger.debug("Command {} execution in progress", command)

    case Done(command, event) if command == c =>
      p.success(event)

    case (command: Command, events: Seq[Event] @unchecked) if command == c =>
      p.success(events.head)
  }
}

case class GoOn(command: Command)