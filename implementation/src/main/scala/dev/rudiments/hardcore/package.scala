package dev.rudiments

import dev.rudiments.hardcore.types.DTO

import scala.language.implicitConversions
import scala.reflect.ClassTag

package object hardcore {

  trait Message extends DTO {
    def toEither[E <: Event]: Result[E] = Left(this)
    def expecting[E <: Event : ClassTag]: Result[E] = this match {
      case it: E => Right(it)
      case other => Left(other)
    }
  }
  trait Command extends Message {}
  trait Event extends Message {
    override def toEither[E <: Event]: Result[E] = Right(this.asInstanceOf[E])
  }
  type Result[E <: Event] = Either[Message, E]
  type Skill[E <: Event] = PartialFunction[Command, Result[E]]

  type MessageProcessor = PartialFunction[Message, Message]

  trait Error extends Event {
    override def toEither[E <: Event]: Either[Message, E] = Left(this)
  }
  object Error {
    case object NoHandler extends Error
    case object NotImplemented extends Error
    case class Internal(throwable: Throwable) extends Error
  }
}
