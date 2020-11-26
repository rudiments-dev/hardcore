package dev.rudiments.data

import dev.rudiments.domain.{ID, Instance}
import dev.rudiments.hardcore._

import scala.collection.mutable

class TransactionalState(persistance: State) extends Skill {

  private val events: mutable.Queue[DataEvent] = mutable.Queue.empty
  private val conclusion: mutable.Map[ID, DataEvent] = mutable.Map.empty

  val f: Skill = {
    case command@Count(All) =>
      persistance(command).flatMap[Counted] { counted =>
        val total = events.foldLeft(counted.total) { (total, event) =>
          event match {
            case Created(key, value) => total + 1
            case Updated(key, oldValue, newValue) => total
            case Moved(oldKey, oldValue, newKey, newValue) => total
            case Deleted(key, value) => total - 1
          }
        }
        Counted(total)
      }
    case command@Find(key) =>
      lastDataEvent(key) match {
        case Some(event) =>
          event match {
            case Created(key, value) => Found(key, value)
            case Updated(key, oldValue, newValue) => Found(key, newValue)
            case Moved(oldKey, oldValue, newKey, newValue) => NotFound(oldKey) //todo if
            case Deleted(key, value) => NotFound(key)
          }
        case None => persistance(command)
      }
    case command@FindAll(All) =>
      persistance(command).flatMap[FoundAll] { found =>
        val values = mutable.Map.empty[ID, Instance]
        values ++= found.content

        conclusion.foreach { case (_, event) =>
          event match {
            case Created(key, value) => values += key -> value
            case Updated(key, _, newValue) => if (values.contains(key)) {
              values += key -> newValue
            } else values += key -> newValue
            case Moved(oldKey, _, newKey, newValue) =>
              values -= oldKey
              values += newKey -> newValue
            case Deleted(key, _) => values -= key
          }
        }
        FoundAll(values.toMap)
      }
    case command@FindAll(p) =>
      persistance(command).flatMap[FoundAll] { found =>
        val values = mutable.Map.empty[ID, Instance]
        values ++ found.content

        conclusion.foreach { case (_, event) =>
          event match {
            case Created(key, value) =>
              if(value.matches(p)) {
                values += (key -> value)
              }
            case Updated(key, _, newValue) => if (values.contains(key)) {
              if (newValue.matches(p)) {
                values(key) = newValue
              } else values -= key
            } else if (newValue.matches(p)) {
              values += (key -> newValue)
            }
            case Moved(oldKey, _, newKey, newValue) =>
              values -= oldKey
              if (newValue.matches(p)) {
                values + (newKey -> newValue)
              }
            case Deleted(key, _) => values -= key
          }
        }
        FoundAll(values.toMap)
      }
    case Create(key, value) =>
      lastDataEvent(key).flatMap {
        case Created(key, created) => Some(AlreadyExists(key, created))
        case Updated(key, oldValue, updated) => Some(AlreadyExists(key, updated))
        case _ => None
      }.getOrElse {
        persistance(Find(key)) match {
          case Found(key, value) => AlreadyExists(key, value)
          case NotFound(key) => Created(key, value)
        }
      }
    case Update(key, value) =>
      lastDataEvent(key).flatMap {
        case _: Deleted | _: Moved => Some(NotFound(key))
        case Created(key, oldValue) => Some(Updated(key, oldValue, value))
        case _ => None
      }.getOrElse {
        persistance(Find(key)) match {
          case Found(key, found) => Updated(key, found, value)
          case NotFound(key) => NotFound(key)
        }
      }
    case Move(oldKey, newKey, value) => {
      //todo care of Moved to newKey from another id than old id
      val transactional: Option[DataError] = (lastDataEvent(oldKey), lastDataEvent(newKey)) match {
        case (Some(_: Deleted | _: Moved), _) => Some(NotFound(oldKey))
        case (_, Some(Created(key, value))) => Some(AlreadyExists(key, value))
        case _ => None
      }
      transactional.getOrElse {
        (persistance(Find(oldKey)), persistance(Find(newKey))) match {
          case (Found(oldKey, oldInstance), NotFound(_)) => Moved(oldKey, oldInstance, newKey, value)
          case (NotFound(_), _) => NotFound(oldKey)
          case (Found(_, _), Found(_, found)) => AlreadyExists(newKey, found)
        }
      }
    }
    case Delete(key) =>
      lastDataEvent(key).flatMap {
        case _: Deleted | _: Moved => Some(NotFound(key))
        case Created(key, value) => Some(Deleted(key, value))
        case Updated(key, oldValue, newValue) => Some(Deleted(key, newValue))
        case _ => None
      }.getOrElse {
        persistance(Find(key)) match {
          case Found(key, value) => Deleted(key, value)
          case NotFound(key) => NotFound(key)
        }
      }
    case CreateAll(batch) =>
      if (batch.forall { case (id, _) => f(Find(id)).isInstanceOf[NotFound]}) {
        Commit(batch.map { case (id, value) => id -> Created(id, value)})
      } else {
        BatchFailed()
      }
    case ReplaceAll(batch) =>
      f(FindAll(All)).flatMap[FoundAll] { found =>
        val batchIds = batch.keySet

        val updates = found.content.view.filterKeys(batchIds.contains).map { case (id, oldInstance) =>
          id -> Updated(id, oldInstance, batch(id))
        }

        val deletes = found.content.view.filterKeys(id => !batchIds.contains(id)).map { case (id, oldInstance) =>
          id -> Deleted(id, oldInstance)
        }

        val creates = (batchIds -- found.content.keySet).map { id =>
          id -> Created(id, batch(id))
        }

        Commit((updates ++ deletes ++ creates).toMap)
      }
    case DeleteUsing(All) =>
      f(FindAll(All)).flatMap[FoundAll] { found =>
        Commit(
          found.content.map { case (id, instance) =>
            id -> Deleted(id, instance)
          }
        )
      }
  }


  def memorize(event: DataEvent): Unit = {
    event match {
      case Commit(state) =>
        state.foreach {case (id, instance) => memorize(instance)}
      case evt@DataEvent(id) =>
        conclusion.get(id) match {
          case Some(existing) if existing == evt => existing
          case Some(existing) =>
            conclusion(id) = evt
            events += evt
            conclusion(id)
          case None =>
            conclusion += id -> evt
            events += evt
            conclusion(id)
        }
    }
  }

  override def apply(ask: Ask): Reply = {
    f.apply(ask) match {
      case evt@DataEvent(id) =>
        memorize(evt)
        evt
      case evt@Commit(state) =>
        memorize(evt)
        evt
      case other => other
    }
  }

  override def isDefinedAt(x: Ask): Boolean = f.isDefinedAt(x)

  private def lastDataEvent(id: ID): Option[DataEvent] = {
    conclusion.get(id)
  }
}
