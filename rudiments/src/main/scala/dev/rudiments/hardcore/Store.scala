package dev.rudiments.hardcore

import scala.collection.mutable
import scala.reflect.ClassTag

class Store[T : ClassTag] extends PartialFunction[(In, Tx), Out] with Location[T, T] {
  val skills: Map[ID[In], Skill] = Seq(
    SagaSkill[Create[T, T], Created[T, T]]((cmd: Create[T, T], tx: Tx) => {
      this.f(Read(cmd.key), tx)
        .|> [NotFound[T, T]] { _ =>
          content.put(cmd.key, cmd.value)
          this.f(Read(cmd.key), tx)
            .|> [Readen[T, T]] { found => Created(cmd.key, found.value) }
            .|> [NotFound[T, T]] { _ => FailedToCreate(cmd.key, cmd.value) }
        }
        .|> [Readen[T, T]] { found => AlreadyExists(cmd.key, found.value) }
    }),
    SagaSkill[Read[T, T], Readen[T, T]] { in: Read[T, T] =>
      content.get(in.key) match {
        case Some(value) => Readen(in.key, value)
        case None => NotFound[T, T](in.key)
      }
    },
    SagaSkill[Update[T, T], Updated[T, T]] { (cmd: Update[T, T], tx: Tx) =>
      this.f(Read(cmd.key), tx) |> [Readen[T, T]] { found =>
        if(found.value == cmd.value) {
          found
        } else {
          content.put(cmd.key, cmd.value)
          this.f(Read(cmd.key), tx) |> [Readen[T, T]] { updated =>
            if(cmd.value == updated.value) {
              Updated(cmd.key, found.value, updated.value)
            } else {
              FailedToUpdate(cmd.key, updated.value)
            }
          }
        }
      }
    },
    SagaSkill[Delete[T, T], Deleted[T, T]] { (cmd: Delete[T, T], tx: Tx) =>
      this.f(Read(cmd.key), tx) |> [Readen[T, T]] { found =>
        content -= cmd.key
        this.f(Read(cmd.key), tx)
          .|> [NotFound[T, T]] { _ => Deleted(cmd.key, found.value) }
          .|> [Readen[T, T]] { failed => FailedToDelete(cmd.key, failed) }
      }
    },
    SagaSkill[Count[T, T], Counted[T, T]] { _: Count[T, T] => Counted[T, T](content.size) },
    SagaSkill[Find[T, T], Found[T, T]] { q: Find[T, T] =>
      q match {
        case Find(All) => Found[T, T](content.toMap)
        case Find(p) => Found[T, T](content.filter { it => matches(it._2, p) }.toMap[ID[T], T])
      }
    },
    SagaSkill[Move[T, T], Moved[T, T]] { (cmd: Move[T, T], tx: Tx) =>
      this.f(Read(cmd.oldKey), tx) |> [Readen[T, T]] { from =>
        this.f(Read(cmd.newKey), tx)
          .|> [NotFound[T, T]] { _ =>
            content.put(cmd.newKey, cmd.value)
            content -= cmd.oldKey
            Moved(cmd.oldKey, from.value, cmd.newKey, cmd.value)
          }
          .|> [Readen[T, T]] { found => AlreadyExists(cmd.newKey, found.value) }
      }
    },
    SagaSkill[Copy[T, T], Copied[T, T]] { (cmd: Copy[T, T], tx: Tx) =>
      this.f(Read(cmd.oldKey), tx) |> [Readen[T, T]] { from =>
        this.f(Read(cmd.newKey), tx)
          .|> [NotFound[T, T]] { _ =>
            content.put(cmd.newKey, cmd.value)
            Copied(cmd.oldKey, from.value, cmd.newKey, cmd.value)
          }
          .|> [Readen[T, T]] { found => AlreadyExists(cmd.newKey, found.value) }
      }
    },
    SagaSkill[CreateAll[T, T], Commit[T, T]] { cmd: CreateAll[T, T] =>
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
    },
    SagaSkill[DeleteUsing[T, T], Commit[T, T]] { cmd: DeleteUsing[T, T] =>
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
    SagaSkill[Reconcile[T, T], Commit[T, T]] { cmd: Reconcile[T, T] =>
      val to = cmd.to
      val create: Map[ID[T], Created[T, T]] = (to -- content.keys).map { case (id, value) => id -> Created[T, T](id, value) }
      val delete: Map[ID[T], Deleted[T, T]] = (content -- to.keys).map { case (id, value) => id -> Deleted[T, T](id, value) }.toMap
      val update: Map[ID[T], Updated[T, T]] = to.view.filterKeys(content.contains).map   {
        case (id, value) if value != content(id) => id -> Updated[T, T](id, content(id), value)
      }.toMap
      Commit[T, T](create ++ update ++ delete)
    },
    SagaSkill[ReplaceAll[T, T], Commit[T, T]] { (cmd: ReplaceAll[T, T], tx: Tx) =>
      this.f(Reconcile[T, T](cmd.batch), tx) |> [Commit[T, T]] { c =>
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
  private val content: mutable.Map[ID[T], T] = mutable.Map.empty

  private def matches(data: T, p: Predicate): Boolean = p match {
    case All => true
    case _ => false
  }

  def apply[I <: In](in: I): Out = this.apply(in, new Tx)
}
