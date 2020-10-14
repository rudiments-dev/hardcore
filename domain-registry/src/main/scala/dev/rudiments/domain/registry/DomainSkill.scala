package dev.rudiments.domain.registry

import dev.rudiments.data.CRUD._
import dev.rudiments.data.ReadOnly._
import dev.rudiments.domain._
import dev.rudiments.hardcore.http.query.interop.InMemoryQueryExecutor
import dev.rudiments.hardcore.{Command, Message, Skill}


class DomainSkill(val ctx: DomainContext) extends Skill {
  override def isDefinedAt(cmd: Command): Boolean = f.isDefinedAt(cmd)
  override def apply(cmd: Command): Message = f.apply(cmd)

  val f: Skill = {
    case Count() => Counted(ctx.domain.model.size)

    case FindAll(query) => FoundAll(
      InMemoryQueryExecutor(query)(ctx.domain.model.values.toList.map(ctx.instanceOf))
    )

    case Find(id@ID(Seq(name: String))) => ctx.domain.model.get(name) match {
      case Some(value) => Found(id, ctx.instanceOf(value))
      case None => NotFound(id)
    }

    case Create(id@ID(Seq(name: String)), instance) => ctx.domain.model.get(name) match {
      case None =>
        ctx.domain.model.put(name, ctx.thingOf(instance))
        ctx.domain.model.get(name) match {
          case Some(created) => Created(id, ctx.instanceOf(created))
          case None => FailedToCreate(id, instance)
        }
      case Some(v) => AlreadyExists(id, ctx.instanceOf(v))
    }

    case Update(id@ID(Seq(name: String)), instance) => ctx.domain.model.get(name) match {
      case Some(found) =>
        val value = ctx.thingOf(instance)
        ctx.domain.model.put(name, value)
        ctx.domain.model.get(name) match {
          case Some(v) if v == value => Updated(id, ctx.instanceOf(found), ctx.instanceOf(v))
          case Some(v) if v != value => FailedToUpdate(id, ctx.instanceOf(found))
          case None => NotFound(id) //TODO think about this error
        }
      case None => NotFound(id)
    }

    case Delete(id@ID(Seq(name: String))) => ctx.domain.model.get(name) match {
        case Some(found) =>
          ctx.domain.model -= name
          ctx.domain.groups -= name
          ctx.domain.model.get(name) match {
            case None => Deleted(id, ctx.instanceOf(found))
            case Some(_) => FailedToDelete(id, ctx.instanceOf(found))
          }
        case None => NotFound(id)
      }

    case d: DomainCommand => ctx(d)

    case other => ???
  }
}
