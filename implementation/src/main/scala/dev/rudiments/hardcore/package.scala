package dev.rudiments

import dev.rudiments.data.Action
import dev.rudiments.domain.{DTO, ID}
import dev.rudiments.hardcore.Error.NoHandler

import scala.language.implicitConversions
import scala.reflect.ClassTag

package object hardcore {

  trait Command

  trait Message extends DTO {
    def flatMap[M <: Message : ClassTag](f: M => Message): Message = this match {
      case m: M => f(m)
      case other => other
    }

    def also[M <: Message : ClassTag](f: M => Unit): this.type = this match {
      case m: M =>
        f(m)
        this
      case other => this
    }
  }

  trait Event extends Message
  trait Error extends Message
  object Error {
    case object NoHandler extends Error
    case object NotImplemented extends Error
    case class Internal(throwable: Throwable) extends Error
  }

  trait Memorable extends Message
  abstract class One(val id: ID) extends Memorable
  abstract class Bulk(val ids: Seq[ID]) extends Memorable
  abstract class All extends Memorable //TODO Predicate(Query?) for search commands

  type Skill = PartialFunction[Command, Message]

  val noSkill: Skill = { case _ => NoHandler }

  object Skill {
    def fromActions(actions: Action[_ <: Command, _ <: Event]*): Skill = {
      new Skill {

        val possibleCommands: Seq[Class[_]] = actions.map(_.commandType)
        override def isDefinedAt(x: Command): Boolean = possibleCommands.contains(x.getClass)

        override def apply(v1: Command): Message = {
          actions
            .find(_.commandType == v1.getClass)
            .map(_.runCommand(v1))
            .getOrElse(NoHandler)
        }
      }
    }

    def fromSkills(skills: Skill*): Skill = {
      skills.foldRight(noSkill)((s, accum) => s.orElse(accum))
    }
  }

}
