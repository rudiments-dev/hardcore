package dev.rudiments.domain

import dev.rudiments.data.CRUD._
import dev.rudiments.data.ReadOnly._
import dev.rudiments.data.DataEvent
import dev.rudiments.hardcore.http.query.interop.InMemoryQueryExecutor
import dev.rudiments.hardcore.{Command, Result, Skill}

class DomainSkill extends Skill[DataEvent] {
  val domain: Domain = Domain()
  domain.makeFromScala[Thing, Thing]
  domain.makeFromScala[Thing, Instance]

  val spec: Spec = domain.find[Spec]("Spec")
  val the: Spec = domain.find[Spec]("The")
  val abs: Spec = domain.find[Spec]("Abstract")
  val valueSpec: Spec = domain.find[Spec]("ValueSpec")

  val instanceOf: PartialFunction[Thing, Instance] = {
    case a: Abstract => abs.fromProduct(domain, a)
    case t: The => the.fromProduct(domain, t)
    case s: Spec => spec.fromProduct(domain, s)
    case other => throw new IllegalArgumentException(s"Unexpected Thing $other")
  }

  val thingOf: PartialFunction[Instance, Thing] = {
    case i if i.spec == abs => i.toScala
    case i if i.spec == the => i.toScala
    case i if i.spec == spec => i.toScala
    case other => throw new IllegalArgumentException(s"Unexpected Instance $other")
  }

  override def isDefinedAt(cmd: Command): Boolean = f.isDefinedAt(cmd)
  override def apply(cmd: Command): Result[DataEvent] = f.apply(cmd)

  val f: PartialFunction[Command, Result[DataEvent]] = {
    case Count() => Counted(domain.model.size).toEither

    case FindAll(query) => FoundAll(
      InMemoryQueryExecutor(query)(domain.model.values.toList.map(instanceOf))
    ).toEither

    case Find(id@ID(Seq(name: String))) => domain.model.get(name) match {
      case Some(value) => Found(id, instanceOf(value)).toEither
      case None => NotFound(id).toEither
    }

    case Create(id@ID(Seq(name: String)), instance) => domain.model.get(name) match {
      case None =>
        domain.model.put(name, thingOf(instance))
        domain.model.get(name) match {
          case Some(created) => Created(id, instanceOf(created)).toEither
          case None => FailedToCreate(id, instance).toEither
        }
      case Some(v) => AlreadyExists(id, instanceOf(v)).toEither
    }

    case Update(id@ID(Seq(name: String)), instance) => domain.model.get(name) match {
      case Some(found) =>
        val value = thingOf(instance)
        domain.model.put(name, value)
        domain.model.get(name) match {
          case Some(v) if v == value => Updated(id, instanceOf(found), instanceOf(v)).toEither
          case Some(v) if v != value => FailedToUpdate(id, instanceOf(found)).toEither
          case None => NotFound(id).toEither //TODO think about this error
        }
      case None => NotFound(id).toEither
    }

    case Delete(id@ID(Seq(name: String))) => domain.model.get(name) match {
        case Some(found) =>
          domain.model -= name
          domain.groups -= name
          domain.model.get(name) match {
            case None => Deleted(id, instanceOf(found)).toEither
            case Some(_) => FailedToDelete(id, instanceOf(found)).toEither
          }
        case None => NotFound(id).toEither
      }

    case other => ???
  }
}
