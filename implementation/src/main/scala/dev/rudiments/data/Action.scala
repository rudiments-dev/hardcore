package dev.rudiments.data

import dev.rudiments.hardcore.{Ask, Reply}

import scala.reflect.ClassTag

abstract class Action[C <: Ask : ClassTag, E <: Reply] {

  val commandType: Class[_] = implicitly[ClassTag[C]].runtimeClass
  def apply(command: C): Reply

  def runCommand(ask: Ask): Reply = {
    apply(ask.asInstanceOf[C])
  }
  
}
