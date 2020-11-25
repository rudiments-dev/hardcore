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
        values ++ found.content

        conclusion.foreach { case (_, event) =>
          event match {
            case Created(key, value) => values + (key -> value)
            case Updated(key, _, newValue) => if (values.contains(key)) {
              values(key) = newValue
            } else values + (key -> newValue)
            case Moved(oldKey, _, newKey, newValue) =>
              values -= oldKey
              values + (newKey -> newValue)
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
//    case CreateAll(batch) =>
//      batch.flatMap {case (id, instance) =>
//        lastDataEvent(id) match {
//          case Some(value) =>
//            value match {
//              case Created(key, value) => Some(BatchFailed())
//              case Updated(key, oldValue, newValue) => Some(BatchFailed())
//              case Moved(oldKey, oldValue, newKey, newValue) => None
//              case Deleted(key, value) => None
//            }
//          case None => None
//        }
//      }
//          try {
//            if((batch -- events.keys).size != batch.size) {
//              BatchFailed()
//            } else {
//              events ++= batch
//              Commit(batch.map { case (id, value) => id -> Created(id, value) })
//            }
//
//          } catch {
//            case _: Exception => BatchFailed()
//          }
//    case ReplaceAll(batch) =>
    //      f(Reconcile(batch)).flatMap[Commit] { c =>
    //        events --= events.keysIterator
    //        events ++= batch
    //        c
    //      }
//    case DeleteUsing(All) =>
//          try {
//            val delete = events.map { case (id, value) => id -> Deleted(id, value) }.toMap
//            events --= events.keysIterator
//            Commit(delete)
//          } catch {
//            case _: Exception => BatchFailed()
//          }

//    case Reconcile(to) =>
    //      val create = (to -- events.keys).map { case (id, value) => id -> Created(id, value) }
    //      val delete = (events -- to.keys).map { case (id, value) => id -> Deleted(id, value) }
    //      val update = to.filterKeys(events.contains).map   {
    //        case (id, value) if value != events(id) => id -> Updated(id, events(id), value)
    //      }
    //      Commit(create ++ update ++ delete)

//    case Apply(commit) => ???

  }


//  def memorize()

  override def apply(ask: Ask): Reply = {
    f.apply(ask) match {
      case evt@DataEvent(id) =>
        conclusion.get(id) match {
          case Some(existing) if existing == evt => existing
          case None =>
            conclusion += id -> evt
            events += evt
            conclusion(id)
        }
      case other => other
    }
  }

  override def isDefinedAt(x: Ask): Boolean = f.isDefinedAt(x)

  private def lastDataEvent(id: ID): Option[DataEvent] = {
    conclusion.get(id)
  }
}
