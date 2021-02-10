package dev.rudiments.another.hardcore


import dev.rudiments.another
import dev.rudiments.another.{In, NoTx, Out, Tx}

import scala.collection.mutable

final class State[T] extends PF {
  private val content: mutable.Map[ID[T], T] = mutable.Map.empty

  private val reconcile = new SagaSkill[Reconcile[T], Tx, Commit[T]]({
    case Reconcile(to) =>
      val create = (to -- content.keys).map { case (id, value) => id -> Created(id, value) }
      val delete = (content -- to.keys).map { case (id, value) => id -> Deleted(id, value) }
      val update = to.view.filterKeys(content.contains).map   {
        case (id, value) if value != content(id) => id -> Updated(id, content(id), value)
      }
      Commit(create ++ update ++ delete)
  })

  private def matches(data: T, p: Predicate): Boolean = p match {
    case All => true
    case _ => false
  }

  private val skills: Seq[PF] = Seq(
    new SagaSkill[Count, Tx, Counted]({ case Count(All) => Counted(content.size) }),
    new SagaSkill[Find[T], Tx, Found[T]]({
      case Find(key) =>
        content.get(key) match {
          case Some(value) => Found(key, value)
          case None => NotFound[T](key)
        }
    }),
    new SagaSkill[FindAll[T], Tx, FoundAll[T]]({
      case FindAll(All) => FoundAll[T](content.toMap)
      case FindAll(p) => FoundAll(content.filter { it => matches(it._2, p) }.toMap[ID[T], T])
    }),
    new SagaSkill[Create[T], Tx, Created[T]]({
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
    }),
    new SagaSkill[Update[T], Tx, Updated[T]]({
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
    }),
    new SagaSkill[Move[T], Tx, Moved[T]]({
      case Move(oldKey, newKey, value) =>
        (content.get(oldKey), content.get(newKey)) match {
          case (Some(found), None) =>
            content -= oldKey
            content.put(newKey, value)
            Moved(oldKey, found, newKey, value)
          case (None, _) => NotFound(oldKey)
          case (Some(_), Some(found)) => AlreadyExists(newKey, found)
        }
    }),
    new SagaSkill[Delete[T], Tx, Deleted[T]]({
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
    }),
    new SagaSkill[CreateAll[T], Tx, Commit[T]]({
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
    }),
    new SagaSkill[ReplaceAll[T], Tx, Commit[T]]({
      case ReplaceAll(batch) =>
        reconcile(Reconcile(batch), NoTx).flatMap[Commit[T]] { c => //TODO propagate Tx?
          content --= content.keysIterator
          content ++= batch
          c
        }
    }),
    new SagaSkill[DeleteUsing, Tx, Commit[T]]({
      case DeleteUsing(All) =>
        try {
          val delete = content.map { case (id, value) => id -> Deleted(id, value) }.toMap
          content --= content.keysIterator
          Commit(delete)
        } catch {
          case _: Exception => BatchFailed()
        }
    }),
    reconcile
  )

  private val composite = new CompositeSkill(skills)
  override val f: PartialFunction[(another.In, Tx), another.Out] = composite.f
  override val signature: Seq[(ID[another.In], ID[another.Out])] = composite.signature

  def apply[I <: In](in: I): Out = this.apply(in, NoTx)
}
