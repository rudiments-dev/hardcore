package dev.rudiments.memory

import dev.rudiments.data.Batch._
import dev.rudiments.data.CRUD._
import dev.rudiments.data.ReadOnly._
import dev.rudiments.domain.{ID, Instance}
import dev.rudiments.hardcore.http.query.interop.InMemoryQueryExecutor
import dev.rudiments.hardcore._

import scala.collection.mutable

class Memory extends Skill[Event] {

  val story: mutable.ArrayBuffer[(Command, Event)] = mutable.ArrayBuffer.empty
  val conclusion: mutable.Map[ID, Event] = mutable.Map.empty
  val state: mutable.Map[ID, Instance] = mutable.Map.empty

  override def isDefinedAt(cmd: Command): Boolean = f.isDefinedAt(cmd)

  override def apply(cmd: Command): Result[Event] = f(cmd).toEither

  def recall(commit: Commit, source: Command): Commit = {
    commit.state.foreach { case(k, v) => memorize(k, source, v) }
    commit
  }

  def memorize(key: ID, cmd: Command, evt: Event): Event = {
    conclusion += key -> evt
    story += cmd -> evt
    conclusion(key)
  }

  val f: PartialFunction[Command, Event] = {
    case Count() => Counted(state.size)

    case Find(key) => state.get(key) match {
      case Some(found) => Found(key, found)
      case None => NotFound(key)
    }

    case FindAll(query) => FoundAll(InMemoryQueryExecutor(query)(state.values.toSeq))

    case cmd@Create(key, value) => state.get(key) match {
      case None =>
        state += key -> value
        memorize(key, cmd, Created(key, state(key)))

      case Some(found) => AlreadyExists(key, found)
    }

    case cmd@Update(key, value) => state.get(key) match {
      case Some(found) if found != value =>
        state += key -> value
        memorize(key, cmd, Updated(key, found, value))

      case Some(found) if found == value => Found(key, value)
      case None => NotFound(key)
    }

    case cmd@Delete(key) =>state.get(key) match {
      case Some(found) =>
        state -= key
        memorize(key, cmd, Deleted(key, found))

      case None => NotFound(key)
    }

    case cmd@CreateAll(batch) =>
      if((batch -- state.keys).size != batch.size) {
        BatchFailed()
      } else {
        state ++= batch
        recall(Commit(batch.map { case (id, value) => id -> Created(id, value) }), cmd)
      }

    case cmd@DeleteAll() =>
      val delete = state.map { case (id, value) => id -> Deleted(id, value) }.toMap
      state --= state.keysIterator
      recall(Commit(delete), cmd)

    case Reconcile(to) =>
      val create = (to -- state.keys.toSet).map { case (id, value) => id -> Created(id, value) }
      val delete = (state -- to.keys).map { case (id, value) => id -> Deleted(id, value) }
      val update = to.filterKeys(state.contains).map   {
        case (id, value) if value == state(id) => id -> Found(id, value)
        case (id, value) if value != state(id) => id -> Updated(id, state(id), value)
      }
      Commit(create ++ update ++ delete)

    case cmd@ReplaceAll(batch) =>
      this.f(Reconcile(batch)) match {
        case c: Commit =>
          state --= state.keysIterator
          state ++= batch
          recall(c, cmd)
        case other => BatchFailed()
      }

    case _ => ???

    //TODO Restore(Commit)
  }
}