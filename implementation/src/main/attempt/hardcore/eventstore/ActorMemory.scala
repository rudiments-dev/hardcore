package dev.rudiments.hardcore.eventstore

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.typesafe.scalalogging.StrictLogging
import dev.rudiments.hardcore.dsl
import dev.rudiments.hardcore.dsl.{Command, Event, Memory, Skill, NoHandler, NoSkill, PF1, PF2, Resolver, SkillSet}

import scala.collection.mutable
import scala.concurrent.{Future, Promise}

class ActorMemory()(implicit val system: ActorSystem) extends Memory with SkillSet {
  val ref: ActorRef = system.actorOf(MemoryActor.props(this))
  private var skills: Skill = NoSkill
  override def skill: Skill = skills

  override def state(): Future[Seq[Event]] = viaPromise(Events)
  override def commands(): Future[Map[Command, Event]] = viaPromise(Commands)
  override def state(command: Command): Future[Option[Event]] = viaPromise(By(command, _))
  override def countEvents(): Future[Long] = viaPromise(Count)

  private def viaPromise[A, T](f: Promise[T] => A): Future[T] = {
    val promise = Promise[T]
    ref ! f(promise)
    promise.future
  }

  override def withSkill(g: PF1): ActorMemory = {
    skills = skills orElse skill(g)
    this
  }

  override def withDependency(r: Resolver)(g: PF2): ActorMemory = {
    skills = skills.orElse(dependent(r)(g))
    this
  }

  override def skill(f: PF1): ActorHardSkill = new ActorHardSkill(f)(this)
  override def dependent(r: Resolver)(f: PF2): DependentActorSkill = new DependentActorSkill(f, r)(this)
}


class MemoryActor(ts: SkillSet) extends Actor with StrictLogging {
  private val events = mutable.ArrayBuffer.empty[Event]
  private val commands = mutable.Map.empty[Command, Event]

  import ActorAction._
  import MemoryActor._
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
        case Some(exists) => sender() ! Done(dependency, exists)

        case None if ts.isDefinedAt(dependency) =>
          logger.debug("Resolving dependency {}", dependency)
          ts.async(dependency)
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
    context.system.eventStream.publish(Done(command, event))
  }
}

object MemoryActor {
  case class ReadyToDo(command: Command)
  case class Requires(command: Command)
  case class Complete(command: Command, event: Event)

  def props(ts: SkillSet): Props = Props(new MemoryActor(ts))
}

private case class Events(promise: Promise[Seq[Event]])
private case class Commands(promise: Promise[Map[Command, Event]])
private case class By(command: Command, promise: Promise[Option[Event]])
private case class Count(promise: Promise[Long])

