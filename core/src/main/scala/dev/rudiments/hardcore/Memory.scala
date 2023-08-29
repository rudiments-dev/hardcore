package dev.rudiments.hardcore

import scala.collection.mutable
import scala.reflect.ClassTag

class Memory { // global context
  //TODO nodes + schemas = structure
  //TODO types
  import Location.*
  val values: mutable.Map[ID, Any] = mutable.Map.empty

  def read(key: Location): Readen | NotFound = {
    key match {
      case id: ID => values.get(id) match {
        case Some(v) => Readen(v)
        case None => NotFound(id)
      }
      case p: Path => NotFound(p)
    }
  }

  def create(key: Location, value: Any): Response = key match {
    case id: ID => values.get(id) match {
      case Some(v) => Conflict(Created(v), Readen(v))
      case None => values += id -> value; Created(value)
    }
    case p: Path => NotFound(p)
  }
  def update(key: Location, oldValue: Any, newValue: Any): Response = key match {
    case id: ID => values.get(id) match {
      case Some(v) if v == oldValue => values += id -> newValue; Updated(v, newValue)
      case Some(v) => Conflict(Updated(v, newValue), Readen(v))
      case None => NotFound(id)
    }
    case p: Path => NotFound(p)
  }
  def delete(key: Location, oldValue: Any): Response = key match {
    case id: ID => values.get(id) match {
      case Some(v) if v == oldValue => values -= id; Deleted(v)
      case Some(v) => Conflict(Deleted(oldValue), Readen(v))
      case None => NotFound(id)
    }
    case p: Path => NotFound(p)
  }


  def /(s: String): Reading = new Reading(this, ID(s))
  def /?(s: String): Response = this.read(ID(s))
  def -(s: String): Response = {
    val k = ID(s)
    this.read(k) match {
      case nf@NotFound(_) => nf
      case Readen(r) => this.delete(k, r)
    }
  }
}

class Reading(mem: Memory, key: Location) {
  def +(value: Any) : Response = mem.read(key) match {
    case NotFound(_) => mem.create(key, value)
    case r => Conflict(Created(value), r)
  }
  def *(value: Any): Response = mem.read(key) match {
    case nf@NotFound(_) => nf
    case Readen(r) => mem.update(key, r, value)
  }
}

enum Location {
  case ID(s: String)
  case Path(ids: Seq[String])
}