package dev.rudiments.hardcore.eventstore

import dev.rudiments.hardcore.dsl._

import scala.concurrent.{Future, Promise}
import scala.language.postfixOps


class ActorTask(val f: Handler)(implicit val es: ActorEventStore)
  extends Task {

  override def future(command: Command): Future[Event] = {
    if(!f.isDefinedAt(command)) throw new MatchError(command)

    val promise = Promise[Event]
    val ref = es.system.actorOf(ActorAction.props(f, command, promise))
    es.subscribe(ref)
    es.ref.!(command)(ref) //TODO refactor
    promise.future
  }
}

object ActorTask {
  def apply(f: Handler)(implicit es: ActorEventStore): Task =
    new ActorTask(f)(es)
}