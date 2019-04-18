package dev.rudiments.hardcore.eventstore

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.typesafe.scalalogging.StrictLogging
import dev.rudiments.hardcore.dsl.{Command, Done, Event, EventStore, InProgress}

import scala.collection.mutable
import scala.concurrent.{Future, Promise}

class ActorEventStore(implicit val system: ActorSystem) extends EventStore {
  val ref: ActorRef = system.actorOf(Props(new EventListenerActor))

  def state(): Future[Seq[Event]] = viaPromise(Events)
  def commands(): Future[Map[Command, Event]] = viaPromise(Commands)
  def state(command: Command): Future[Option[Event]] = viaPromise(By(command, _))
  def countEvents(): Future[Long] = viaPromise(Count)

  def subscribe(ref: ActorRef): Unit = {
    system.eventStream.subscribe(ref, classOf[Done])
  }

  def unsubscribe(ref: ActorRef): Unit = {
    system.eventStream.unsubscribe(ref)
  }

  private def viaPromise[A, T](f: Promise[T] => A): Future[T] = {
    val promise = Promise[T]
    ref ! f(promise)
    promise.future
  }
}


class EventListenerActor extends Actor with StrictLogging {
  private var events = mutable.ArrayBuffer.empty[Event]
  private val commands = mutable.Map.empty[Command, Event]

  override def receive: Receive = {
    case command: Command =>
      commands.get(command) match {
        case Some(InProgress) =>
          sender() ! (command, InProgress)
        case Some(exists) =>
          sender() ! Done(command, exists)
        case None =>
          commands.put(command, InProgress)
          sender() ! GoOn(command)
      }
      logger.trace("Command received {}", command)

    case (command: Command, event: Event) =>
      events += event
      commands.put(command, event)
      publish(command, event)
      logger.trace("Event received {} on command {}", event, command)

    case Events(promise) => promise.success(events)
    case Commands(promise) => promise.success(commands.toMap)
    case By(command: Command, promise) => promise.success(commands.toMap.get(command))
    case Count(promise) => promise.success(events.size)
    case other => logger.warn("Received {} of type {}", other, other.getClass.getSimpleName)
  }

  def publish(command: Command, event: Event): Unit = {
    context.system.eventStream.publish(Done(command, event))
  }
}

case class Events(promise: Promise[Seq[Event]])
case class Commands(promise: Promise[Map[Command, Event]])
case class By(command: Command, promise: Promise[Option[Event]])
case class Count(promise: Promise[Long])