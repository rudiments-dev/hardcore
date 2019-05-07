package dev.rudiments.hardcore.eventstore

import akka.actor.{Actor, Props}
import com.typesafe.scalalogging.StrictLogging
import dev.rudiments.hardcore.dsl._

import scala.concurrent.{Future, Promise}

class DependentActorTask(val f: PF2, val r: Resolver)(implicit val es: ActorEventStore)
  extends DependentTask {

  override def async(command: Command): Future[Event] = {
    val promise = Promise[Event]
    es.system.actorOf(DependentActorAction.props(f, command, r(command), promise)(es))
    promise.future
  }
}


class DependentActorAction(
  val f: PF2,
  val command: Command,
  val dependency: Command,
  val promise: Promise[Event]
)(implicit es: ActorEventStore)
  extends Actor with StrictLogging with DependentAction {

  import EventStoreActor._
  import ActorAction._

  private var canFire = false
  es.ref ! ReadyToDo(command)
  es.system.eventStream.subscribe(self, classOf[Done])
  override def receive: Receive = {
    case GoOn(c) if command == c =>
      canFire = true
      es.ref ! Requires(dependency)

    case InProgress(c) if command == c =>
      logger.debug("Command {} execution in progress", c)

    case InProgress(d) if dependency == d =>
      logger.debug("Dependency {} execution in progress", d)

    case Done(c, event) if command == c =>
      promise.success(event)

    case Done(d, event) if dependency == d && canFire =>
      val result = f((command, event))
      context.system.eventStream.unsubscribe(self)
      es.ref ! Complete(command, result)
      promise.success(result)

  }
}

object DependentActorAction {
  def props(
    f: PF2,
    command: Command,
    dependency: Command,
    promise: Promise[Event]
  )(implicit es: ActorEventStore): Props =
    Props(new DependentActorAction(f, command, dependency, promise))
}