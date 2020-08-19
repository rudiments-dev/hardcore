package dev.rudiments.types.hard

import dev.rudiments.types.{ValueSpec, Thing, Type}
import scala.reflect.runtime.universe._

class ScalaType[T : TypeTag] (
  name: String,
  fields: Map[String, ValueSpec],
  ascendants: Seq[Thing] = Seq.empty
) extends Type(name, fields, ascendants) {
  def constructScala(arguments: Any*): T = {
    val c = Class.forName(typeOf[T].typeSymbol.asClass.fullName)
    c.getConstructors()(0).newInstance(arguments.map(_.asInstanceOf[Object]): _*).asInstanceOf[T]
  }
}