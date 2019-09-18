package dev.rudiments.hardcore.types

import com.sun.tools.javac.code.TypeTag
import dev.rudiments.hardcore.DTO

import scala.reflect.runtime.universe._

case class Type[T <: DTO](name: String, fields: Seq[Field])
object Type {
  def apply[T: TypeTag]: Type[T] = {
    val fields = Seq.empty[Field]
    new Type[T](typeOf[T].typeSymbol.name.toString.trim, fields)
  }
}

case class Field(name: String, optional: Boolean)

