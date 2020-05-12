package dev.rudiments.hardcore

package object flow {

  sealed trait ControlCommand extends Command {}

  case object Pause                   extends ControlCommand
  case object Continue                extends ControlCommand
  case class ContinueWith(evt: Event) extends ControlCommand
  case object Retry                   extends ControlCommand
  case object Rollback                extends ControlCommand


  sealed trait ControlEvent extends Event {
    override def toEither: Either[Message, Event] = Left(this)
  }
  case object Waiting     extends ControlEvent
  case object InProgress  extends ControlEvent
  case object Paused      extends ControlEvent


  sealed trait ControlError extends ControlEvent with Error {}

  case object FailedToPause     extends ControlError
  case object FailedToContinue  extends ControlError
  case object FailedToRetry     extends ControlError
  case object FailedToRollback  extends ControlError
}
