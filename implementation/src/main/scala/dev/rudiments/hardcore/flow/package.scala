package dev.rudiments.hardcore

import dev.rudiments.types.ID

package object flow {

  sealed trait ControlCommand extends Command {}

  case object Pause                   extends ControlCommand
  case object Continue                extends ControlCommand
  case class ContinueWith(evt: Event) extends ControlCommand
  case class Retry(cmd: Command)      extends ControlCommand
  case object Rollback                extends ControlCommand


  sealed trait ControlEvent extends Event {
    override def toEither[E <: Event]: Result[E] = Failure(this)
  }
  case object Waiting     extends ControlEvent
  case object InProgress  extends ControlEvent
  case object Paused      extends ControlEvent


  sealed trait ControlError extends ControlEvent with Error {}

  case object FailedToPause               extends ControlError
  case object FailedToContinue            extends ControlError
  case class FailedToRetry(cmd: Command)  extends ControlError
  case object FailedToRollback            extends ControlError

  trait AlwaysDo
  trait SideEffect
  trait CacheSingle { val key: ID }
  trait ReadSingle extends CacheSingle
  trait BulkRead
  trait Mutates extends CacheSingle
  trait Mutated extends CacheSingle
  trait BulkMutate
  trait BulkMutated
}
