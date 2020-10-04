package dev.rudiments.domain.registry

import dev.rudiments.data.CRUD._
import dev.rudiments.data.ReadOnly._
import dev.rudiments.domain._
import dev.rudiments.hardcore.http.query.interop.InMemoryQueryExecutor
import dev.rudiments.hardcore.{Command, Event, Result, Skill}

import scala.collection.immutable.ListMap

sealed trait DomainCommand extends Command
case class CreateThing(name: String) extends DomainCommand
case class RenameThing(oldName: String, newName: String) extends DomainCommand
case class DeleteThing(name: String) extends DomainCommand

case class ThingIs(thing: ThingRef, parent: ThingRef) extends DomainCommand
case class ThingIsNot(thing: ThingRef, parent: ThingRef) extends DomainCommand

case class AddField(thing: ThingRef, name: String, spec: Thing, required: Boolean) extends DomainCommand
case class RenameField(thing: ThingRef, oldName: String, newName: String) extends DomainCommand
case class ChangeField(thing: ThingRef, name: String, newThing: Thing) extends DomainCommand
case class MakeFieldRequired(thing: ThingRef, name: String) extends DomainCommand
case class MakeFieldOptional(thing: ThingRef, name: String) extends DomainCommand
case class ReorderFields(thing: ThingRef, order: Seq[String]) extends DomainCommand
case class RemoveField(thing: ThingRef, name: String) extends DomainCommand


sealed trait DomainEvent extends Event
case class ThingCreated(name: String, thing: Thing) extends DomainEvent
case class ThingRenamed(oldName: String, newName: String, thing: Thing) extends DomainEvent
case class ThingDeleted(name: String, thing: Thing) extends DomainEvent

case class NowThingIs(thing: ThingRef, is: Set[String]) extends DomainEvent
case class ThingIsAlready(thing: ThingRef, is: ThingRef) extends DomainEvent
case class ThingIsAlreadyNot(thing: ThingRef, is: ThingRef) extends DomainEvent

case class FieldAdded(thing: Thing, name: String) extends DomainEvent
case class FieldAlreadyExists(thing: Thing, name: String) extends DomainEvent
case class FieldNotFound(thing: Thing, name: String) extends DomainEvent

case class FieldRenamed(thing: Thing, oldName: String, newName: String) extends DomainEvent
case class FieldChanged(thing: Thing, name: String, newThing: Thing) extends DomainEvent
case class FieldAlreadyAThing(thing: Thing, name: String) extends DomainEvent
case class NowFieldRequired(thing: Thing, name: String) extends DomainEvent
case class FieldAlreadyRequired(thing: Thing, name: String) extends DomainEvent
case class NowFieldOptional(thing: Thing, name: String) extends DomainEvent
case class FieldAlreadyOptional(thing: Thing, name: String) extends DomainEvent
case class FieldsReordered(thing: Thing, order: Seq[String]) extends DomainEvent
case class FieldsAlreadyInOrder(thing: Thing) extends DomainEvent
case class FieldsNotOrdered(thing: Thing, extra: Set[String], missing: Set[String]) extends DomainEvent
case class FieldRemoved(thing: Thing, name: String, field: ValueSpec) extends DomainEvent

class DomainSkill extends Skill[Event] {
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
  override def apply(cmd: Command): Result[Event] = f.apply(cmd)

  val f: PartialFunction[Command, Result[Event]] = {
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

    case CreateThing(name) => domain.model.get(name) match {
      case None =>
        domain.save(Abstract(name, ListMap.empty), Set.empty)
        ThingCreated(name, domain.find[Abstract](name)).toEither
      case Some(thing) =>
        AlreadyExists(ID(Seq(name)), instanceOf(thing)).toEither
    }

    case RenameThing(oldName, newName) => domain.model.get(oldName) match {
      case Some(thing) =>
        domain.model -= oldName
        domain.model += newName -> thing
        domain.groups += newName -> domain.groups(oldName)
        domain.groups -= oldName
        ThingRenamed(oldName, newName, thing).toEither
      case None =>
        NotFound(ID(Seq(oldName))).toEither
    }

    case DeleteThing(name) => domain.model.get(name) match {
      case Some(thing) =>
        domain.model -= name
        domain.groups -= name
        ThingDeleted(name, thing).toEither
      case None =>
        NotFound(ID(Seq(name))).toEither
    }

    case ThingIs(thing, parent) => domain.groups.get(thing.name) match {
      case Some(parents) if parents.contains(parent.name) =>
        ThingIsAlready(thing, parent).toEither
      case Some(parents) =>
        domain.groups += thing.name -> (parents + parent.name)
        NowThingIs(thing, domain.groups(thing.name)).toEither
      case None =>
        NotFound(ID(Seq(thing.name))).toEither
    }

    case ThingIsNot(thing, parent) => domain.groups.get(thing.name) match {
      case Some(parents) if parents.contains(parent.name) =>
        domain.groups += thing.name -> (parents - parent.name)
        NowThingIs(thing, domain.groups(thing.name)).toEither
      case Some(_) =>
        ThingIsAlreadyNot(thing, parent).toEither
      case None =>
        NotFound(ID(Seq(thing.name))).toEither
    }

    case AddField(thing, name, spec, required) => domain.model.get(thing.name) match {
      case Some(t) =>
        val field = name -> ValueSpec(spec, required)
        val updated = t match {
          case a: Abstract => a.copy(fields = a.fields + field)
          case s: Spec => s.copy(fields = s.fields + field)
          case _: The => Abstract(thing.name, ListMap(field))
          case _ => ???
        }
        domain.model += thing.name -> updated
        FieldAdded(updated, name).toEither //TODO read field from domain
      case None =>
        NotFound(ID(Seq(name))).toEither
    }

    case RenameField(thing, oldName, newName) => domain.model.get(thing.name) match {
      case None =>
        NotFound(ID(Seq(thing.name))).toEither
      case Some(t) =>
        t match {
          case a: Abstract => a.fields.get(oldName) match {
            case None =>
              FieldNotFound(a, oldName).toEither
            case Some(f) =>
              val updated = a.copy(fields = a.fields - oldName + (newName -> f))
              domain.model += thing.name -> updated
              FieldRenamed(domain.model(thing.name), oldName, newName).toEither
          }
          case s: Spec => s.fields.get(oldName) match {
            case None =>
              FieldNotFound(s, oldName).toEither
            case Some(f) =>
              val updated = s.copy(fields = s.fields - oldName + (newName -> f))
              domain.model += thing.name -> updated
              FieldRenamed(domain.model(thing.name), oldName, newName).toEither
          }
          case other => ???
        }
    }

    case ChangeField(thing, name, newThing) => domain.model.get(thing.name) match {
      case None =>
        NotFound(ID(Seq(thing.name))).toEither
      case Some(t) =>
        val updated = t match {
          case a: Abstract =>
            val field = a.fields(name).copy(thing = newThing)
            a.copy(fields = a.fields + (name -> field))
          case s: Spec =>
            val field = s.fields(name).copy(thing = newThing)
            s.copy(fields = s.fields + (name -> field))
          case other => ???
        }
        domain.model += thing.name -> updated
        FieldChanged(domain.model(thing.name), name, updated).toEither
    }

    case MakeFieldRequired(thing, name) => domain.model.get(thing.name) match {
      case None =>
        NotFound(ID(Seq(thing.name))).toEither
      case Some(t) =>
         t match {
          case a: Abstract =>
            if(a.fields(name).isRequired) {
              FieldAlreadyRequired(a, name).toEither
            } else {
              val field = a.fields(name).copy(isRequired = true)
              val updated = a.copy(fields = a.fields + (name -> field))
              domain.model += thing.name -> updated
              NowFieldRequired(domain.model(thing.name), name).toEither
            }
          case s: Spec =>
            if(s.fields(name).isRequired) {
              FieldAlreadyRequired(s, name).toEither
            } else {
              val field = s.fields(name).copy(isRequired = true)
              val updated = s.copy(fields = s.fields + (name -> field))
              domain.model += thing.name -> updated
              NowFieldRequired(domain.model(thing.name), name).toEither
            }
          case other => ???
        }
    }

    case MakeFieldOptional(thing, name) => domain.model.get(thing.name) match {
      case None =>
        NotFound(ID(Seq(thing.name))).toEither
      case Some(t) =>
        t match {
          case a: Abstract =>
            if(!a.fields(name).isRequired) {
              FieldAlreadyOptional(a, name).toEither
            } else {
              val field = a.fields(name).copy(isRequired = false)
              val updated = a.copy(fields = a.fields + (name -> field))
              domain.model += thing.name -> updated
              NowFieldOptional(domain.model(thing.name), name).toEither
            }
          case s: Spec =>
            if(!s.fields(name).isRequired) {
              FieldAlreadyOptional(s, name).toEither
            } else {
              val field = s.fields(name).copy(isRequired = false)
              val updated = s.copy(fields = s.fields + (name -> field))
              domain.model += thing.name -> updated
              NowFieldOptional(domain.model(thing.name), name).toEither
            }
          case other => ???
        }
    }

    case ReorderFields(thing, order) => domain.model.get(thing.name) match {
      case None =>
        NotFound(ID(Seq(thing.name))).toEither
      case Some(t) =>
        t match {
          case a: Abstract =>
            val extra = order.toSet -- a.fields.keySet
            val missing = a.fields.keySet -- order.toSet
            if(extra.nonEmpty || missing.nonEmpty) {
              FieldsNotOrdered(a, extra, missing).toEither
            } else if(order == a.fields.keys) {
              FieldsAlreadyInOrder(a).toEither
            } else {
              val updated = a.copy(fields = ListMap(order.map(it => it -> a.fields(it)): _*))
              domain.model += thing.name -> updated
              FieldsReordered(domain.model(thing.name), order).toEither
            }
          case s: Spec =>
            val extra = order.toSet -- s.fields.keySet
            val missing = s.fields.keySet -- order.toSet
            if(extra.nonEmpty || missing.nonEmpty) {
              FieldsNotOrdered(s, extra, missing).toEither
            } else if(order == s.fields.keys) {
              FieldsAlreadyInOrder(s).toEither
            } else {
              val updated = s.copy(fields = ListMap(order.map(it => it -> s.fields(it)): _*))
              domain.model += thing.name -> updated
              FieldsReordered(domain.model(thing.name), order).toEither
            }

          case other => ???
        }
    }

    case RemoveField(thing, name) => domain.model.get(thing.name) match {
      case None =>
        NotFound(ID(Seq(thing.name))).toEither
      case Some(t) =>
        t match {
          case a: Abstract =>
            a.fields.get(name) match {
              case None =>
                FieldNotFound(a, name).toEither
              case Some(found) =>
                val updated = a.copy(fields = a.fields + (name -> found.copy(isRequired = false)))
                domain.model += thing.name -> updated
                NowFieldRequired(domain.model(thing.name), name).toEither
            }
          case s: Spec =>
            s.fields.get(name) match {
              case None =>
                FieldNotFound(s, name).toEither
              case Some(found) =>
                val updated = s.copy(fields = s.fields + (name -> found.copy(isRequired = false)))
                domain.model += thing.name -> updated
                NowFieldRequired(domain.model(thing.name), name).toEither
            }
          case other => ???
        }
    }

    case other => ???
  }
}
