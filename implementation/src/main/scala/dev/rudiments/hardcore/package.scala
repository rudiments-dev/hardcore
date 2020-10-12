package dev.rudiments

import dev.rudiments.data.Action
import dev.rudiments.domain.{DTO, ID}
import dev.rudiments.hardcore.Error.NoHandler

import scala.language.implicitConversions

package object hardcore {



  trait Message extends DTO {
    def toEither[E <: Event]: Either[Message, E] = Left(this)
  }

  trait Command

  trait Event extends Message {
    override def toEither[E <: Event]: Either[Message, E] = Right(this.asInstanceOf[E])
  }

  trait Error extends Message {
    override def toEither[E <: Event]: Either[Message, E] = Left(this)
  }
  object Error {
    case object NoHandler extends Error
    case object NotImplemented extends Error
    case class Internal(throwable: Throwable) extends Error
  }

  trait Memorable extends Message
  abstract class One(val id: ID) extends Memorable
  abstract class Bulk(val ids: Seq[ID]) extends Memorable
  abstract class All extends Memorable //TODO Predicate(Query?) for search commands

  type Result[E <: Event] = Either[Message, E]

  type Skill[E <: Event] = PartialFunction[Command, Result[E]]

  object Skill {
    def fromActions[E <: Event](actions: Action[_ <: Command, _ <: E]*): Skill[E] = {
      new Skill[E] {

        val possibleCommands: Seq[Class[_]] = actions.map(_.commandType)
        override def isDefinedAt(x: Command): Boolean = possibleCommands.contains(x.getClass)

        override def apply(v1: Command): Result[E] = {
          actions
            .find(_.commandType == v1.getClass)
            .map(_.runCommand(v1))
            .getOrElse(Left(NoHandler))
        }
      }
    }
  }

}
