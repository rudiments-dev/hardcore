package dev.rudiments.hardcore.eventstore

import akka.actor.{Actor, Props}
import com.typesafe.scalalogging.StrictLogging
import dev.rudiments.hardcore.dsl._

import scala.concurrent.{Future, Promise}
import scala.language.postfixOps


class ActorHardSkill(val f: PF1)(implicit es: ActorMemory) extends HardSkill {

  override def async(command: Command): Future[Event] = {
    if(!f.isDefinedAt(command)) throw new MatchError(command)
    val promise = Promise[Event]
    es.system.actorOf(ActorAction.props(f, command, promise)(es))
    promise.future
  }
}


class ActorAction(val f: PF1, val command: Command, val promise: Promise[Event])(implicit es: ActorMemory)
  extends Actor with StrictLogging with Action {

  import ActorAction._
  import MemoryActor._
  es.ref ! ReadyToDo(command)
  es.system.eventStream.subscribe(self, classOf[Done])
  override def receive: Receive = {
    case GoOn(c: Command) if command == c =>
      val result = f match {
        case h: HardSkill => h.f(command)
        case _: DependentSkill => ???
        case or: OrElseSkill => or.sync(command)
        case pf: PF1 => pf(command)
      }
      context.system.eventStream.unsubscribe(self)
      es.ref ! Complete(command, result)
      promise.success(result)

    case InProgress(c: Command) if command == c =>
      logger.debug("Command {} execution in progress", command)

    case Done(c, event) if command == c =>
      promise.success(event)
  }
}

object ActorAction {
  case class GoOn(command: Command)
  case class Done(command: Command, event: Event)
  case class InProgress(command: Command)

  def props(f: PF1, command: Command, promise: Promise[Event])(implicit es: ActorMemory): Props =
    Props(new ActorAction(f, command, promise))
}