package dev.rudiments

import dev.rudiments.data.Action
import dev.rudiments.hardcore.Error.NoHandler
import dev.rudiments.types.DTO

import scala.language.implicitConversions
import scala.reflect.ClassTag

package object hardcore {

  sealed abstract class SkillResult[+A, +B] {

    def map[B1](f: B => B1) : SkillResult[A, B1] = this match {
      case dev.rudiments.hardcore.Success(message) => Success(f(message))
      case dev.rudiments.hardcore.Failure(message) => Failure(message)
    }

    def flatMap[A1, B1](f: B => SkillResult[A1, B1]): SkillResult[A1, B1] = this match {
      case dev.rudiments.hardcore.Success(message) => f(message)
      case dev.rudiments.hardcore.Failure(message) => this.asInstanceOf[SkillResult[A1, B1]]
    }

    def transform[A1, B1](f: A => A1, g: B => B1): SkillResult[A1, B1] = this match {
      case Success(message) => Success(g(message))
      case Failure(message) => Failure(f(message))
    }

    def recover[A1](f: A => A1) : SkillResult[A1, B] = this match {
      case dev.rudiments.hardcore.Success(message) => Success(message)
      case dev.rudiments.hardcore.Failure(message) => Failure(f(message))
    }

    def filter[F](p: B => Boolean): Option[SkillResult[F, B]] = this match {
      case x @ Success(a) if p(a) => Some(x.asInstanceOf[SkillResult[F, B]])
      case _                      => None
    }

    def expecting[T <: Event : ClassTag]: SkillResult[Message, T] = this match {
      case dev.rudiments.hardcore.Success(message) => message match {
        case e: T => Success(e)
        case _ => Failure(message).asInstanceOf[SkillResult[Message, T]]
      }
      case dev.rudiments.hardcore.Failure(message) => message match {
        case e: T => Success(e)
        case _ => Failure(message).asInstanceOf[SkillResult[Message, T]]
      }
    }
  }

  object SkillResult {
    implicit class Mergeable[T](private val x: SkillResult[T, T]) extends AnyVal {
      def merge: T = x match {
        case Success(a) => a
        case Failure(a) => a
      }
    }
  }

  case class Success[+A, +B](message: B) extends SkillResult[A, B]
  case class Failure[+A, +B](message: A) extends SkillResult[A, B]

  trait Message extends DTO {
    def toEither[E <: Event]: SkillResult[Message, E] = Failure(this)
  }
  trait Command extends Message {}
  trait Event extends Message {
    override def toEither[E <: Event]: SkillResult[Message, E] = Success(this.asInstanceOf[E])
  }
  type Result[E <: Event] = SkillResult[Message, E]
  type Skill[E <: Event] = PartialFunction[Command, Result[E]]

  object Skill {
    def fromActions[E <: Event](actions: Action[_ <: Command, _ <: E]*): Skill[E] = {
      new Skill[E] {

        val possibleCommands: Seq[Class[_]] = actions.map(_.commandType)
        override def isDefinedAt(x: Command): Boolean = possibleCommands.contains(x.getClass)

        override def apply(v1: Command): Result[E] = {
          actions.find(_.commandType == v1.getClass).map(_.runCommand(v1))
            .getOrElse(Failure(NoHandler))
        }
      }
    }
  }

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
