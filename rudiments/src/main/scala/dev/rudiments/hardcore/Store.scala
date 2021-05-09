package dev.rudiments.hardcore

import scala.collection.mutable

class Store[T] extends PartialFunction[(In, Tx), Out] with Location[T, T] {
  val f: PartialFunction[(In, Tx), Out] = {
    case (_: Count[T, T], _) => Counted[T, T](content.size)
    case (q: Read[T, T], _) => //TODO add Tx into output of read value
      content.get(q.key) match {
        case Some(value) => Readen(q.key, value)
        case None => NotFound[T, T](q.key)
      }
    case (Find(All), tx: LogTx) => Found[T, T](content.toMap)
    case (Find(p), tx: LogTx) => Found[T, T](content.filter { it => matches(it._2, p) }.toMap[ID[T], T])

    case (cmd: Create[T, T], tx: LogTx) =>
      content.get(cmd.key) match {
        case None =>
          content.put(cmd.key, cmd.value)
          content.get(cmd.key) match {
            case Some(created) => Created(cmd.key, created)
            case None => FailedToCreate(cmd.key, cmd.value)
          }
        case Some(v) => AlreadyExists(cmd.key, v)
      }

    case (cmd: Update[T, T], tx: LogTx) =>
      content.get(cmd.key) match {
        case Some(found) =>
          content.put(cmd.key, cmd.value)
          content.get(cmd.key) match {
            case Some(v) if v == cmd.value => Updated(cmd.key, found, cmd.value)
            case Some(v) if v != cmd.value => FailedToUpdate(cmd.key, v)
            case None => NotFound(cmd.key) //TODO think about this error
          }
        case None => NotFound(cmd.key)
      }

    case (cmd: Delete[T, T], tx: LogTx) =>
      content.get(cmd.key) match {
        case Some(found) =>
          content -= cmd.key
          content.get(cmd.key) match {
            case None => Deleted(cmd.key, found)
            case Some(_) => FailedToDelete(cmd.key, found)
          }
        case None => NotFound(cmd.key)
      }

    case (cmd: Move[T, T], tx: LogTx) =>
      (content.get(cmd.oldKey), content.get(cmd.newKey)) match {
        case (Some(found), None) =>
          content -= cmd.oldKey
          content.put(cmd.newKey, cmd.value)
          Moved(cmd.oldKey, found, cmd.newKey, cmd.value)
        case (None, _) => NotFound(cmd.oldKey)
        case (Some(_), Some(found)) => AlreadyExists(cmd.newKey, found)
      }

    case (cmd: Copy[T, T], tx: LogTx) =>
      (content.get(cmd.oldKey), content.get(cmd.newKey)) match {
        case (Some(found), None) =>
          content.put(cmd.newKey, cmd.value)
          Copied(cmd.oldKey, found, cmd.newKey, cmd.value)
        case (None, _) => NotFound(cmd.oldKey)
        case (Some(_), Some(found)) => AlreadyExists(cmd.newKey, found)
      }

    case (cmd: CreateAll[T, T], tx: LogTx) =>
      try {
        if((cmd.batch -- content.keys).size != cmd.batch.size) {
          BatchFailed()
        } else {
          content ++= cmd.batch
          Commit(cmd.batch.map { case (id, value) => id -> Created[T, T](id, value) })
        }
      } catch {
        case _: Exception => BatchFailed()
      }

    case (DeleteUsing(All), tx: LogTx) =>
      try {
        val delete = content.map { case (id, value) => id -> Deleted(id, value) }.toMap
        content --= content.keysIterator
        Commit(delete)
      } catch {
        case _: Exception => BatchFailed()
      }

    case (cmd: Reconcile[T, T], tx: LogTx) =>
      val to = cmd.to
      val create: Map[ID[T], Created[T, T]] = (to -- content.keys).map { case (id, value) => id -> Created[T, T](id, value) }
      val delete: Map[ID[T], Deleted[T, T]] = (content -- to.keys).map { case (id, value) => id -> Deleted[T, T](id, value) }.toMap
      val update: Map[ID[T], Updated[T, T]] = to.view.filterKeys(content.contains).map   {
        case (id, value) if value != content(id) => id -> Updated[T, T](id, content(id), value)
      }.toMap
      Commit[T, T](create ++ update ++ delete)

    case (cmd: ReplaceAll[T, T], tx: LogTx) =>
      this.f(Reconcile[T, T](cmd.batch), tx).flatMap[Commit[T, T]] { c =>
        content --= content.keysIterator
        content ++= cmd.batch
        c
      }
  }

  override def isDefinedAt(x: (In, Tx)): Boolean = f.isDefinedAt(x)
  override def apply(x: (In, Tx)): Out = f.apply(x)
  private val content: mutable.Map[ID[T], T] = mutable.Map.empty

  private def matches(data: T, p: Predicate): Boolean = p match {
    case All => true
    case _ => false
  }

  def apply[I <: In](in: I): Out = this.apply(in, new LogOnlyTx)
}
