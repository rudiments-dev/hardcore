package dev.rudiments.hardcore.eventstore

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.typesafe.scalalogging.StrictLogging
import dev.rudiments.hardcore.dsl
import dev.rudiments.hardcore.dsl.{Command, Event, EventStore, HardTask, NoHandler, NoTask, NotImplemented, PF1, PF2, Resolver, TaskStore}

import scala.collection.mutable
import scala.concurrent.{Future, Promise}

class ActorEventStore()(implicit val system: ActorSystem) extends EventStore with TaskStore {
  val ref: ActorRef = system.actorOf(EventStoreActor.props(this))
  private var task: HardTask = NoTask
  override def f: HardTask = task

  override def state(): Future[Seq[Event]] = viaPromise(Events)
  override def commands(): Future[Map[Command, Event]] = viaPromise(Commands)
  override def state(command: Command): Future[Option[Event]] = viaPromise(By(command, _))
  override def countEvents(): Future[Long] = viaPromise(Count)

  private def viaPromise[A, T](f: Promise[T] => A): Future[T] = {
    val promise = Promise[T]
    ref ! f(promise)
    promise.future
  }

  override def withTask(g: PF1): ActorEventStore = {
    val hard: HardTask = g match {
      case t: HardTask => task.orElse(t)
      case pf: PF1 => new ActorTask(pf)(this)
    }
    task = task orElse hard
    this
  }

  override def withDependency(r: Resolver)(g: PF2): ActorEventStore = {
    task = task.orElse(new DependentActorTask(g, r)(this))
    this
  }

  override def future(command: Command): Future[Event] = {
    f.future(command)
  }

  override def task(f: PF1): ActorTask = new ActorTask(f)(this)
  override def future(f: PF1, command: Command): Future[Event] = {
    val promise = Promise[Event]
    implicit val callback: ActorRef = system.actorOf(ActorAction.props(f, command, promise)(this))
    promise.future
  }

  override def dependent(r: Resolver)(f: PF2): DependentActorTask = new DependentActorTask(f, r)(this)
  override def future(f: PF2, command: Command, dependency: Command): Future[Event] = {
    val promise = Promise[Event]
    system.actorOf(DependentActorAction.props(f, command, dependency, promise)(this))
    promise.future
  }

  override def complete(command: Command, result: Event): Unit = {
    ref ! EventStoreActor.Complete(command, result)
  }
}


class EventStoreActor(ts: TaskStore) extends Actor with StrictLogging {
  private val events = mutable.ArrayBuffer.empty[Event]
  private val commands = mutable.Map.empty[Command, Event]

  import ActorAction._
  import EventStoreActor._
  override def receive: Receive = {
    case ReadyToDo(command) =>
      commands.get(command) match {
        case Some(dsl.InProgress) => sender() ! InProgress(command)
        case Some(exists) => sender() ! Done(command, exists)
        case None =>
          commands.put(command, dsl.InProgress)
          sender() ! GoOn(command)
      }
      logger.trace("Command received {}", command)

    case Complete(command, event) =>
      events += event
      commands.put(command, event)
      publish(command, event)
      logger.trace("Event received {} on command {}", event, command)

    case Requires(dependency) =>
      logger.debug("Required {}", dependency)
      commands.get(dependency) match {
        case Some(dsl.InProgress) => sender() ! InProgress(dependency)
        case Some(exists) => sender() ! ActorAction.Done(dependency, exists)

        case None if ts.isDefinedAt(dependency) =>
          logger.debug("Resolving dependency {}", dependency)
          ts.future(dependency)
          sender() ! InProgress(dependency)

        case None if !ts.isDefinedAt(dependency) =>
          commands.put(dependency, NoHandler(dependency))
          publish(dependency, NoHandler(dependency))
      }

    case Events(promise) => promise.success(events)
    case Commands(promise) => promise.success(commands.toMap)
    case By(command, promise) => promise.success(commands.get(command))
    case Count(promise) => promise.success(events.size)

    case other => logger.warn("Received {} of type {}", other, other.getClass.getSimpleName)
  }

  def publish(command: Command, event: Event): Unit = {
    context.system.eventStream.publish(ActorAction.Done(command, event))
  }
}

object EventStoreActor {
  case class ReadyToDo(command: Command)
  case class Requires(command: Command)
  case class Complete(command: Command, event: Event)

  def props(ts: TaskStore): Props = Props(new EventStoreActor(ts))
}

private case class Events(promise: Promise[Seq[Event]])
private case class Commands(promise: Promise[Map[Command, Event]])
private case class By(command: Command, promise: Promise[Option[Event]])
private case class Count(promise: Promise[Long])

