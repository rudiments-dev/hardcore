package dev.rudiments.data

import dev.rudiments.hardcore.{All, Ask, Query, Reply, Skill}

final class SegregatedState(query: State) extends Skill {
  private val command: State = new State

  val f: Skill = {

    case cmd: Query => query(cmd)

    case cmd@Create(key, value) => command(Find(key))
      .flatMap { _: NotFound => //TODO handle 'deleted locally' case
        query(Find(key)).flatMap[NotFound] { _ => command(cmd) }
      }.flatMap { found: Found =>
        AlreadyExists(key, found.value)
      }

    case cmd@Update(key, value) => command(cmd).flatMap { _: NotFound =>
      query(Find(key)).flatMap { _: Found =>
        command(Create(key, value)) //TODO take memory from source?
      }
    }

    case cmd@Move(oldKey, newKey, value) =>
      command(cmd).flatMap { _: NotFound =>
        query(Find(oldKey)).flatMap { it: Found =>
          query(Find(newKey)).flatMap { _: NotFound =>
            command(Find(newKey)).flatMap { _: NotFound =>
              command(Create(it.key, it.value))
              command(cmd)
            }
          }.flatMap { found: Found =>
            AlreadyExists(found.key, found.value)
          }
        }
      }

    case cmd@Delete(key) => command(cmd).flatMap { _: NotFound =>
      query(Find(key)).flatMap { it: Found =>
        Deleted(it.key, it.value) //TODO take history from query
        //Do nothing else, because command(Find(key)) should return NotFound
      }
    }

    case cmd@CreateAll(batch) =>
      //TODO fix work with ID across query and command States. Possible switch to Memory
      query(FindAll(All)).flatMap { it: FoundAll =>
        if((batch -- it.content.keySet).size != batch.size) {
          BatchFailed()
        } else {
          command(cmd)
        }
      }
    case cmd@ReplaceAll(batch) => command(cmd)//TODO fix work with ID across query and command States. Possible switch to Memory
    case cmd@DeleteUsing(All) =>
      query(FindAll(All)).flatMap { it1: FoundAll =>
        command(FindAll(All)).flatMap { _: FoundAll =>
          val delete = it1.content.map { case (id, i) => id -> Deleted(id, i) }
          command(cmd).flatMap { it2: Commit =>
            Commit(delete ++ it2.state)
          }
        }
      }

    case Apply(commit) => ???

  }

  override def apply(ask: Ask): Reply = f.apply(ask)

  override def isDefinedAt(x: Ask): Boolean = f.isDefinedAt(x)
}