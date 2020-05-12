package dev.rudiments.hardcore.flow

import dev.rudiments.hardcore._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Future, Promise}

class ControlFlow {
  private case class PromiseListener(pf: MessageProcessor, p: Promise[Message])

  val memory: mutable.Queue[(Command, Message)] = mutable.Queue.empty[(Command, Message)]
  private val listeners: ArrayBuffer[PromiseListener] = mutable.ArrayBuffer.empty

  def put(cmd: Command, msg: Message): Unit = {
    memory += cmd -> msg
    listeners.foreach {
      case i@PromiseListener(pf, p) =>
        if(pf.isDefinedAt(msg)) {
          p.success(pf(msg))
          listeners -= i
        }
    }
  }

  def lastMessage(command: Command): Option[Message] = memory.collect {
    case (cmd, evt) if (cmd == command) => evt
  }.lastOption

  def asMap: Map[Command, Seq[Message]] = memory.groupBy(_._1).mapValues(i => i.map(_._2))

  def waitFor(pf: MessageProcessor): Future[Message] = {
    val p = Promise[Message]()
    memory.collect {
      case (_, msg) if pf.isDefinedAt(msg) => pf(msg)
    }.lastOption match {
      case Some(msg) => p.success(msg)
      case None => listeners.+=(PromiseListener(pf, p))
    }
    p.future
  }
}
