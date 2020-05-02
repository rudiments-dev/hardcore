package dev.rudiments

import dev.rudiments.hardcore.types.DTO

import scala.language.implicitConversions

package object hardcore {

  trait Message extends DTO {
    def toEither: Either[Message, Event] = Left(this)
    def expecting[R]: Either[Message, R] = this match {
      case it: R if it.isInstanceOf[R] => Right(it)
      case other => Left(other)
    }
  }
  trait Command extends Message {}
  trait Effect extends Message {}
  trait Event extends Effect {
    override def toEither: Either[Message, Event] = Right(this)
  }
  type Skill = PartialFunction[Command, Event]
  type HardSkill[C <: Command, E <: Event] = PartialFunction[C, E]

  type MessageProcessor = PartialFunction[Message, Message]

  trait Error extends Event {
    override def toEither: Either[Message, Event] = Left(this)
  }
  object Error {
    case object NoHandler extends Error
    case object NotImplemented extends Error
    case class Internal(throwable: Throwable) extends Error
  }

  implicit def toMessage(value: Either[Message, Event]): Message = value match {
    case Left(msg) => msg
    case Right(evt) => evt
  }

  implicit def asSkill[C <: Command, E <: Event](from: HardSkill[C, E]): Skill = {
    case cmd: C if cmd.isInstanceOf[C] && from.isDefinedAt(cmd) => from(cmd)
  }
}
