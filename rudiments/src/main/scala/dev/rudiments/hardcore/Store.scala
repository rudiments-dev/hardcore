package dev.rudiments.hardcore

import scala.collection.mutable
import scala.reflect.ClassTag

class Store[K : ClassTag, V : ClassTag] extends PartialFunction[(In, Tx), Out] with Location[K, V] {
  val read: OptionSkill[V, Read[K, V], Readen[K, V], NotFound[K, V]] = new OptionSkill(
    cmd => content.get(cmd.key),
    (in, _, found) => Readen[K, V](in.key, found),
    (in, _) => NotFound(in.key)
  )


  val skills: Map[ID[In], Skill] = Seq(
    SagaSkill[Create[K, V], Created[K, V]]((cmd: Create[K, V], tx: Tx) => {
      read(Read(cmd.key), tx)
        .|> [NotFound[K, V]] { _ =>
          content.put(cmd.key, cmd.value)
          read(Read(cmd.key), tx)
            .|> [Readen[K, V]] { found => Created(cmd.key, found.value) }
            .|> [NotFound[K, V]] { _ => FailedToCreate(cmd.key, cmd.value) }
        }
        .|> [Readen[K, V]] { found => AlreadyExists(cmd.key, found.value) }
    }),
    read,
    SagaSkill[Update[K, V], Updated[K, V]] { (cmd: Update[K, V], tx: Tx) =>
      read(Read(cmd.key), tx) |> [Readen[K, V]] { found =>
        if(found.value == cmd.value) {
          found
        } else {
          content.put(cmd.key, cmd.value)
          read(Read(cmd.key), tx) |> [Readen[K, V]] { updated =>
            if(cmd.value == updated.value) {
              Updated(cmd.key, found.value, updated.value)
            } else {
              FailedToUpdate(cmd.key, updated.value)
            }
          }
        }
      }
    },
    SagaSkill[Delete[K, V], Deleted[K, V]] { (cmd: Delete[K, V], tx: Tx) =>
      read(Read(cmd.key), tx) |> [Readen[K, V]] { found =>
        content -= cmd.key
        read(Read(cmd.key), tx)
          .|> [NotFound[K, V]] { _ => Deleted(cmd.key, found.value) }
          .|> [Readen[K, V]] { failed => FailedToDelete(cmd.key, failed) }
      }
    },
    SagaSkill[Count[K, V], Counted[K, V]] { _: Count[K, V] => Counted[K, V](content.size) },
    SagaSkill[Find[K, V], Found[K, V]] { q: Find[K, V] =>
      q match {
        case Find(All) => Found[K, V](content.toMap)
        case Find(p) => Found[K, V](content.filter { it => matches(it._2, p) }.toMap[ID[K], V])
      }
    },
    SagaSkill[Move[K, V], Moved[K, V]] { (cmd: Move[K, V], tx: Tx) =>
      read(Read(cmd.oldKey), tx) |> [Readen[K, V]] { from =>
        read(Read(cmd.newKey), tx)
          .|> [NotFound[K, V]] { _ =>
            content.put(cmd.newKey, cmd.value)
            content -= cmd.oldKey
            Moved(cmd.oldKey, from.value, cmd.newKey, cmd.value)
          }
          .|> [Readen[K, V]] { found => AlreadyExists(cmd.newKey, found.value) }
      }
    },
    SagaSkill[Copy[K, V], Copied[K, V]] { (cmd: Copy[K, V], tx: Tx) =>
      read(Read(cmd.oldKey), tx) |> [Readen[K, V]] { from =>
        read(Read(cmd.newKey), tx)
          .|> [NotFound[K, V]] { _ =>
            content.put(cmd.newKey, cmd.value)
            Copied(cmd.oldKey, from.value, cmd.newKey, cmd.value)
          }
          .|> [Readen[K, V]] { found => AlreadyExists(cmd.newKey, found.value) }
      }
    },
    SagaSkill[CreateAll[K, V], Commit[K, V]] { cmd: CreateAll[K, V] =>
      try {
        if((cmd.batch -- content.keys).size != cmd.batch.size) {
          BatchFailed()
        } else {
          content ++= cmd.batch
          Commit(cmd.batch.map { case (id, value) => id -> Created[K, V](id, value) })
        }
      } catch {
        case _: Exception => BatchFailed()
      }
    },
    SagaSkill[DeleteUsing[K, V], Commit[K, V]] { cmd: DeleteUsing[K, V] =>
      cmd match {
        case DeleteUsing(All) =>
          try {
            val delete = content.map { case (id, value) => id -> Deleted(id, value) }.toMap
            content --= content.keysIterator
            Commit(delete)
          } catch {
            case _: Exception => BatchFailed()
          }
      }
    },
    SagaSkill[Reconcile[K, V], Commit[K, V]] { cmd: Reconcile[K, V] =>
      val to = cmd.to
      val create: Map[ID[K], Created[K, V]] = (to -- content.keys).map { case (id, value) => id -> Created[K, V](id, value) }
      val delete: Map[ID[K], Deleted[K, V]] = (content -- to.keys).map { case (id, value) => id -> Deleted[K, V](id, value) }.toMap
      val update: Map[ID[K], Updated[K, V]] = to.view.filterKeys(content.contains).map   {
        case (id, value) if value != content(id) => id -> Updated[K, V](id, content(id), value)
      }.toMap
      Commit[K, V](create ++ update ++ delete)
    },
    SagaSkill[ReplaceAll[K, V], Commit[K, V]] { (cmd: ReplaceAll[K, V], tx: Tx) =>
      this.f(Reconcile[K, V](cmd.batch), tx) |> [Commit[K, V]] { c =>
        content --= content.keysIterator
        content ++= cmd.batch
        c
      }
    }
  ).map(s => s.signature -> s).toMap

  val f: PartialFunction[(In, Tx), Out] = {
    case (in, tx: Tx) => skills.get(ID[In, String](in.getClass.getName)) match {
      case Some(skill) => skill(in, tx)
      case None => NotImplemented(in)
    }
  }

  override def isDefinedAt(x: (In, Tx)): Boolean = f.isDefinedAt(x)
  override def apply(x: (In, Tx)): Out = f.apply(x)
  private val content: mutable.Map[ID[K], V] = mutable.Map.empty

  private def matches(data: V, p: Predicate): Boolean = p match {
    case All => true
    case _ => false
  }

  def apply[I <: In](in: I): Out = this.apply(in, new Tx)
}
