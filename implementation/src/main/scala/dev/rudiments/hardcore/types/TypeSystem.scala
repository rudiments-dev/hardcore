package dev.rudiments.hardcore.types

import scala.collection.mutable

class TypeSystem {
  val types: mutable.Map[String, Thing] = mutable.Map.empty

  def find(name: String): Option[Thing] = {
    types.get(name)
  }

  def save(t: Thing): Thing = {
    types += t.name -> t

    types.values.collect {
      case a@Type(typeName, fields) =>
        fields.collect {
          case Tuple2(fieldName, Field(Types.Reference(LateInit(name)), flag)) if name == t.name =>
            save(a.copy(fields = fields + (fieldName -> Field(Types.Reference(t), flag))))
        }
    }

    t
  }
}
