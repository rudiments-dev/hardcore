package dev.rudiments.data

import dev.rudiments.hardcore.{Command, Event, Result}

import scala.reflect.ClassTag

abstract class Action[C <: Command : ClassTag, E <: Event] {


  val commandType: Class[_] = implicitly[ClassTag[C]].runtimeClass
  def apply(command: C): Result[E]

  def runCommand(command: Command): Result[E] = {
    apply(command.asInstanceOf[C])
  }
  
}
