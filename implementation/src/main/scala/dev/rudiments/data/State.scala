package dev.rudiments.data

import dev.rudiments.domain.{ID, Instance}
import dev.rudiments.hardcore.All
import dev.rudiments.hardcore.{Ask, Reply, Skill}

import scala.collection.mutable

final class State extends Skill {
  private val content: mutable.Map[ID, Instance] = mutable.Map.empty

  val f: Skill = {

    case Count(All) => Counted(content.size)
    case Find(key) =>
      content.get(key) match {
        case Some(value) => Found(key, value)
        case None => NotFound(key)
      }
    case FindAll(All) => FoundAll(content.values.toList)

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
    case Move(oldKey, newKey, value) =>
      (content.get(oldKey), content.get(newKey)) match {
        case (Some(found), None) =>
          content -= oldKey
          content.put(newKey, value)
          Moved(oldKey, found, newKey, value)
        case (None, _) => NotFound(oldKey)
        case (Some(_), Some(found)) => AlreadyExists(newKey, found)
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
        if((batch -- content.keys).size != batch.size) {
          BatchFailed()
        } else {
          content ++= batch
          Commit(batch.map { case (id, value) => id -> Created(id, value) })
        }

      } catch {
        case _: Exception => BatchFailed()
      }
    case ReplaceAll(batch) =>
      f(Reconcile(batch)).flatMap[Commit] { c =>
        content --= content.keysIterator
        content ++= batch
        c
      }
    case DeleteUsing(All) =>
      try {
        val delete = content.map { case (id, value) => id -> Deleted(id, value) }.toMap
        content --= content.keysIterator
        Commit(delete)
      } catch {
        case _: Exception => BatchFailed()
      }

    case Reconcile(to) =>
      val create = (to -- content.keys).map { case (id, value) => id -> Created(id, value) }
      val delete = (content -- to.keys).map { case (id, value) => id -> Deleted(id, value) }
      val update = to.filterKeys(content.contains).map   {
        case (id, value) if value != content(id) => id -> Updated(id, content(id), value)
      }
      Commit(create ++ update ++ delete)

    case Apply(commit) => ???

  }

  override def apply(ask: Ask): Reply = f.apply(ask)

  override def isDefinedAt(x: Ask): Boolean = f.isDefinedAt(x)
}
