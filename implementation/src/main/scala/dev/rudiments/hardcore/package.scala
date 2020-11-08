package dev.rudiments

import dev.rudiments.data.Action
import dev.rudiments.domain.{DTO, ID}
import dev.rudiments.hardcore.Error.NoHandler

import scala.language.implicitConversions
import scala.reflect.ClassTag

package object hardcore {

  trait Message extends DTO

  trait Ask extends Message
  trait Reply extends Message {
    def flatMap[R <: Reply : ClassTag](f: R => Reply): Reply = this match {
      case m: R => f(m)
      case other => other
    }

    def also[M <: Reply : ClassTag](f: M => Unit): this.type = this match {
      case m: M =>
        f(m)
        this
      case _ => this
    }
  }

  trait Command extends Ask
  trait Query extends Ask

  trait Event extends Reply
  trait Report extends Reply
  trait Error extends Reply
  object Error {
    case object NoHandler extends Error
    case object NotImplemented extends Error
    case class Internal(throwable: Throwable) extends Error
  }

  type Skill = PartialFunction[Ask, Reply]

  val noSkill: Skill = { case _ => NoHandler }

  object Skill {
    def fromActions(actions: Action[_ <: Ask, _ <: Reply]*): Skill = {
      new Skill {

        val possibleCommands: Seq[Class[_]] = actions.map(_.commandType)
        override def isDefinedAt(x: Ask): Boolean = possibleCommands.contains(x.getClass)

        override def apply(v1: Ask): Reply = {
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
