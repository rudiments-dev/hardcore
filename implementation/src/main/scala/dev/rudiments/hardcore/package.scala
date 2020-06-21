package dev.rudiments

import dev.rudiments.hardcore.types.DTO

import scala.language.implicitConversions
import scala.reflect.ClassTag

package object hardcore {

  sealed abstract class SkillResult[+A <: Message, +B <: Message] {

    def map[B1 <: Message](f: B => B1) : SkillResult[A, B1] = this match {
      case dev.rudiments.hardcore.Success(message) => Success(f(message))
      case dev.rudiments.hardcore.Failure(message) => Failure(message)
    }

    def flatMap[A1 <: Message,B1 <: Message](f: B => SkillResult[A1, B1]): SkillResult[A1, B1] = this match {
      case dev.rudiments.hardcore.Success(message) => f(message)
      case dev.rudiments.hardcore.Failure(message) => this.asInstanceOf[SkillResult[A1, B1]]
    }

    def expecting[T <: Event : ClassTag]: SkillResult[Message, T] = this match {
      case dev.rudiments.hardcore.Success(message) => message match {
        case e: T => Success[Message, T](e)
        case _ => Failure(message)
      }
      case msg@dev.rudiments.hardcore.Failure(message) => Failure(message)
    }

    def merge(): Message = this match {
      case dev.rudiments.hardcore.Success(message) => message
      case dev.rudiments.hardcore.Failure(message) => message
    }
  }


  case class Success[+A <: Message, +B <: Message](message: B) extends SkillResult[A, B]
  case class Failure[+A <: Message, +B <: Message](message: A) extends SkillResult[A, B]

  trait Message extends DTO {
    def toEither[E <: Event]: SkillResult[Message, E] = Failure(this)
  }
  trait Command extends Message {}
  trait Event extends Message {
    override def toEither[E <: Event]: SkillResult[Message, E] = Success(this.asInstanceOf[E])
  }
  type Result[E <: Event] = SkillResult[Message, E]
  type Skill[E <: Event] = PartialFunction[Command, Result[E]]

  type MessageProcessor = PartialFunction[Message, Message]

  trait Error extends Event {
    override def toEither[E <: Event]: SkillResult[Message, E] = Failure(this)
  }
  object Error {
    case object NoHandler extends Error
    case object NotImplemented extends Error
    case class Internal(throwable: Throwable) extends Error
  }
}
