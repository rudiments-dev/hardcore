package dev.rudiments.hardcore.dsl

import scala.concurrent.Future

trait EventStore {
  def state(): Future[Seq[Event]]
//TODO how to pass implicit params?
//  def willDo(command: Command): Unit
//  def done(command: Command, event: Event): Unit
}
