package dev.rudiments.domain.registry

import dev.rudiments.domain.{Thing, ThingRef, ValueSpec}
import dev.rudiments.hardcore.{Command, Event}

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
