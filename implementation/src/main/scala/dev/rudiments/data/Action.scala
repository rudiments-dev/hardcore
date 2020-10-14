package dev.rudiments.data

import dev.rudiments.hardcore.{Command, Event, Message}

import scala.reflect.ClassTag

abstract class Action[C <: Command : ClassTag, E <: Event] {

  val commandType: Class[_] = implicitly[ClassTag[C]].runtimeClass
  def apply(command: C): Message

  def runCommand(command: Command): Message = {
    apply(command.asInstanceOf[C])
  }
  
}
