package dev.rudiments.hardcore.repo.actor

import dev.rudiments.hardcore.dsl._
import dev.rudiments.hardcore.eventstore.{ActorEventStore, ActorTask}
import dev.rudiments.hardcore.repo.memory.SyncMemoryRepo

import scala.language.postfixOps


class ActorDataHandler[A](implicit meta: Meta[A], es: ActorEventStore)
  extends ActorTask(new SyncMemoryRepo[A]()) {
}