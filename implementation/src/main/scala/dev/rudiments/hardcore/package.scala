package dev.rudiments

import dev.rudiments.hardcore.types.DTO

package object hardcore {

  trait Command extends DTO {}
  trait Effect extends DTO {}
  trait Event extends Effect {}
  type Skill[C <: Command, E <: Event] = PartialFunction[C, E]

  sealed trait LifeCycle extends Effect {}
  object LifeCycle {
    case object Waiting extends LifeCycle
    case object InProgress extends LifeCycle
  }

  trait Error extends Event {}
  object Error {
    case object NoHandler extends Error
    case object NotImplemented extends Error
    case class Internal(throwable: Throwable) extends Error
  }
}
