package dev.rudiments.domain.registry

import dev.rudiments.data.CRUD.AlreadyExists
import dev.rudiments.data.ReadOnly.NotFound
import dev.rudiments.domain.{Abstract, Domain, ID, Instance, Spec, The, Thing, ValueSpec}
import dev.rudiments.hardcore.{Command, Event, Result, Skill}
import dev.rudiments.memory.Memory

import scala.collection.immutable.ListMap

class DomainContext extends Skill[Event] {
  val domain: Domain = Domain()
  val memory: Memory = new Memory

  domain.makeFromScala[Thing, Thing]
  domain.makeFromScala[Thing, Instance]

  val spec: Spec = domain.find[Spec]("Spec")
  val the: Spec = domain.find[Spec]("The")
  val abs: Spec = domain.find[Spec]("Abstract")
  val valueSpec: Spec = domain.find[Spec]("ValueSpec")

  override def apply(cmd: Command): Result[Event] = f(cmd)

  override def isDefinedAt(cmd: Command): Boolean = f.isDefinedAt(cmd)

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
  
  private val f: Skill[Event] = {
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
  }
}
