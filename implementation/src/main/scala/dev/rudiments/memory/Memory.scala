package dev.rudiments.memory

import dev.rudiments.data.Batch.{BatchFailed, Commit, CreateAll, DeleteAll, Reconcile, ReplaceAll, Restore}
import dev.rudiments.data.CRUD._
import dev.rudiments.data.ReadOnly._
import dev.rudiments.data.{DataCommand, DataEvent, DataSkill}
import dev.rudiments.domain.{ID, Instance}
import dev.rudiments.hardcore.http.query.interop.InMemoryQueryExecutor
import dev.rudiments.hardcore.{Adapter, Command, Event, Result, Success}

import scala.collection.mutable

class Memory extends Adapter[DataCommand, DataEvent]{

  val story: mutable.ArrayBuffer[(Command, Event)] = mutable.ArrayBuffer.empty
  val conclusion: mutable.Map[ID, DataEvent] = mutable.Map.empty
  val state: mutable.Map[ID, Instance] = mutable.Map.empty

  override def isDefinedAt(cmd: Command): Boolean = f.isDefinedAt(cmd)

  override def apply(cmd: Command): Result[DataEvent] = f(cmd)

  def recall(commit: Commit, source: Command): Commit = {
    commit.state.foreach { case(k, v) =>
      conclusion += k -> v
      story += source -> v
    }
    commit
  }

  private def commandFromEvent(evt: DataEvent): DataCommand = evt match {
    case Created(id, value) => Create(id, value)
    case Updated(id, _, value) => Update(id, value)
    case Deleted(id, _) => Delete(id)
    case other => throw new IllegalArgumentException(s"Only Created | Updated | Deleted supported in Commit, but $other found")
  }

  private def memorize(key: ID, cmd: Command, evt: DataEvent): DataEvent = {
    conclusion += key -> evt
    story += cmd -> evt
    conclusion(key)
  }

  val f: DataSkill = {
    case cmd: DataCommand =>
      val event: DataEvent = cmd match {
        case Count() => Counted(state.size)

        case Find(key) => state.get(key) match {
          case Some(found) => Found(key, found)
          case None => NotFound(key)
        }

        case FindAll(query) => FoundAll(InMemoryQueryExecutor(query)(state.values.toSeq))

        case Create(key, value) => state.get(key) match {
          case None =>
            state += key -> value
            memorize(key, cmd, Created(key, state(key)))

          case Some(found) => AlreadyExists(key, found)
        }

        case Update(key, value) => state.get(key) match {
          case Some(found) if found != value =>
            state += key -> value
            memorize(key, cmd, Updated(key, found, value))

          case Some(found) if found == value => Found(key, value)
          case None => NotFound(key)
        }

        case Delete(key) =>state.get(key) match {
          case Some(found) =>
            state -= key
            memorize(key, cmd, Deleted(key, found))

          case None => NotFound(key)
        }

        case CreateAll(batch) =>
          if((batch -- state.keys).size != batch.size) {
            BatchFailed()
          } else {
            state ++= batch
            recall(Commit(batch.map { case (id, value) => id -> Created(id, value) }), cmd)
          }

        case DeleteAll() =>
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

        case ReplaceAll(batch) =>
          this.f(Reconcile(batch)) match {
            case Success(c: Commit) =>
              state --= state.keysIterator
              state ++= batch
              recall(c, cmd)
            case other => BatchFailed()
          }

        //TODO Restore(Commit)
      }

      event.toEither
  }
}