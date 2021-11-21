package dev.rudiments.hardcore

import dev.rudiments.hardcore.Memory._

import scala.collection.mutable

class Memory {
  val state: mutable.SeqMap[ID, Data] = mutable.SeqMap.empty
  val readPF: PartialFunction[In, Out] = Memory.read(this)

  val skill: PartialFunction[In, Out] = readPF.orElse(create(this)).orElse(update(this)).orElse(delete(this)) //TODO CRUD Agent from skills

  def read(id: ID): Out = readPF(Read(id))

  def apply(in: In): Out = skill.apply(in)
}

object Memory {
  def read(that: Memory): PartialFunction[In, Out] = {
    case Read(id: ID) => that.state.get(id) match {
      case Some(found) => Readen(id, found)
      case None => NotFound(id)
    }
  }

  def create(that: Memory): PartialFunction[In, Out] = {
    case Create(id, data) => that.read(id) match {
      case Readen(i, found) => AlreadyExist(i, found)
      case NotFound(i) => Created(i, data)
    }
  }

  def update(that: Memory): PartialFunction[In, Out] = {
    case Update(id: ID, data: Data) => that.read(id) match {
      case Readen(i, found) => Updated(i, found, data)
      case e: Error => e
    }
  }

  def delete(that: Memory): PartialFunction[In, Out] = {
    case Delete(id: ID) => that.read(id) match {
      case Readen(i, found) => Deleted(i, found)
      case e: Error => e
    }
  }

  def commit(that: Memory): PartialFunction[In, Out] = {
    case Apply(data) =>
      val out = data.map { case (id, event) =>
        event match {
          case Created(k, v) =>
            that.state += k -> v // TODO error-handling
            k -> Some(v)
          case Updated(k, oldData, newData) =>
            that.state += k -> newData // TODO error-handling
            k -> Some(newData)
          case Deleted(k, v) =>
            that.state -= k // TODO error-handling
            k -> None
        }
      }.collect { case (k, Some(v)) => k -> v }
      Commit(out)
  }
}
