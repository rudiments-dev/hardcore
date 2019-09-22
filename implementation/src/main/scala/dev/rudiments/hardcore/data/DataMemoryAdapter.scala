package dev.rudiments.hardcore.data

import dev.rudiments.hardcore.types._
import dev.rudiments.hardcore.Adapter
import dev.rudiments.hardcore.types.ID.{AutoID, ID1}

import scala.collection.parallel.mutable

class DataMemoryAdapter[T <: DTO : Type] extends Adapter[DataCommand[T], DataEvent[T]] {
  private val content = mutable.ParMap.empty[ID[T], T]
  private var nextAutoID: Long = 0

  override def isDefinedAt(x: DataCommand[T]): Boolean = x match { //TODO generify
    case _: Create[T] => true
    case _: Find[T] => true
    case _: Update[T] => true
    case _: Delete[T] => true
    case _ => false
  }

  override def apply(cmd: DataCommand[T]): DataEvent[T] = cmd match {

    case Create(AutoID(), value) =>
      nextAutoID += 1
      val key = ID1[T, Long](nextAutoID)
      content.put(key, value)
      content.get(key) match {
        case Some(created) => Created(key, created)
        case None => FailedToCreate(key, value)
      }

    case Create(key, value) =>
      content.get(key) match {
        case None =>
          content.put(key, value)
          content.get(key) match {
            case Some(created) => Created(key, created)
            case None => FailedToCreate(key, value)
          }
        case Some(v) => AlreadyExists(key, v)
      }

    case Find(key) =>
      content.get(key) match {
        case Some(value) => Found(key, value)
        case None => NotFound(key)
      }

    case Update(key, value) =>
      content.get(key) match {
        case Some(found) =>
          content.put(key, value)
          content.get(key) match {
            case Some(v) if v == value => Updated(key, found, value)
            case Some(v) if v != value => FailedToUpdate(key, v)
            case None => NotFound(key) //TODO think about this error
          }
        case None => NotFound(key)
      }

    case Delete(key) =>
      content.get(key) match {
        case Some(found) =>
          content -= key
          content.get(key) match {
            case None => Deleted(key, found)
            case Some(_) => FailedToDelete(key, found)
          }
        case None => NotFound(key)
      }

    case CreateAll(batch) =>
      try {
        content ++= batch
        AllCreated(batch)
      } catch {
        case _: Exception => BatchFailed()
      }

    case DeleteAll() =>
      try {
        content.clear()
        AllDeleted()
      } catch {
        case _: Exception => BatchFailed()
      }

    case Count() => Counted(content.size)
  }
}

