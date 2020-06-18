package dev.rudiments.hardcore.flow

import dev.rudiments.data.Batch.{AllCreated, AllDeleted, AllReplaced}
import dev.rudiments.data.CRUD.{Created, Deleted, Updated}
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.types.{DeletedInstance, ID, Instance}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Future, Promise}

class ControlFlow {
  private case class PromiseListener(pf: MessageProcessor, p: Promise[Message])

  val memory: mutable.Queue[(Command, Message)] = mutable.Queue.empty[(Command, Message)]
  val state: mutable.Map[ID, Stateful] = mutable.Map.empty
  private val listeners: ArrayBuffer[PromiseListener] = mutable.ArrayBuffer.empty

  def put(cmd: Command, msg: Message): Unit = {
    memory += cmd -> msg
    (cmd, msg) match {
      case (_, single: CacheSingle) => state += single.key -> Stateful(single.key, cmd, msg)
      case (single: CacheSingle, _) => state += single.key -> Stateful(single.key, cmd, msg)
      case _ =>
    }

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

case class Stateful(
  id: ID,
  memory: mutable.Queue[(Command, Message)],
  var last: (Command, Message),
  var value: Instance
) {
  def mutate(cmd: Command, msg: Message): Stateful = {
    memory += cmd -> msg
    last = cmd -> msg
    (cmd, msg) match {
      case (_, Created(_, v)) => value = v
      case (_, Updated(_, _, v)) => value = v
      case (_, Deleted(_, v)) => value = DeletedInstance
      case (_, AllCreated(batch)) => batch.get(id).foreach { value = _ }
      case (_, AllReplaced(batch)) => batch.get(id) match {
        case Some(v) => value = v
        case None => value = DeletedInstance
      }
      case (_, AllDeleted) => value = DeletedInstance
      case (_, _) => //pass
    }
    this
  }
}
object Stateful {
  def apply(id: ID, cmd: Command, msg: Message): Stateful = new Stateful(
    id,
    mutable.Queue.empty[(Command, Message)],
    null,
    null
  ).mutate(cmd, msg)
}
