package dev.rudiments.hardcore.types

import scala.reflect.runtime.universe._

case class Type[T <: DTO](name: String, fields: Map[String, Field])
object Type {
  def apply[T <: DTO : TypeTag]: Type[T] = {
    new Type[T](
      typeOf[T].typeSymbol.name.toString.trim,
      typeOf[T].members.collect {
        case m: TermSymbol if m.isVal => m
      }.map { f =>
        (f.name.toString.trim, Field(f))
      }.toMap
    )
  }
}

case class Field(kind: FieldType, boxed: Boolean, default: Boolean)
object Field {
  def apply(symbol: TermSymbol): Field = {
    if(symbol.typeSignature.typeSymbol == typeOf[Option[_]].typeSymbol)
      new Field(FieldType(symbol.typeSignature.typeArgs.head), true, symbol.isParamWithDefault)
    else
      new Field(FieldType(symbol.typeSignature), false, symbol.isParamWithDefault)
  }
}
