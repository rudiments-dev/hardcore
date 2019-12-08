package dev.rudiments.hardcore.types

import java.sql.{Date, Time, Timestamp}

import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.{Type => SysType}

case class Type[T <: DTO](name: String, fields: Map[String, Field])
object Type {
  def apply[T <: DTO : TypeTag]: Type[T] = {
    new Type[T](
      typeOf[T].typeSymbol.name.toString.trim,
      typeOf[T].typeSymbol.asClass.primaryConstructor.typeSignature.paramLists(0).collect {
        case m: TermSymbol => (m.name.toString.trim, Field(m))
      }.toMap
    )
  }
}

case class Field(kind: FieldType, fieldFlag: FieldFlag)
object Field {
  def apply(symbol: TermSymbol): Field = {
    if(symbol.typeSignature <:< typeOf[Option[_]]) {
      //TODO check if default == None
      new Field(FieldType(symbol.typeSignature.typeArgs.head), FieldFlags.Optional)
    } else if(symbol.typeSignature <:< typeOf[Iterable[_]]) {
      new Field(FieldType(symbol.typeSignature.typeArgs.head), if(symbol.isParamWithDefault) CollectionFlags.WithDefault else CollectionFlags.CanBeEmpty)
      //TODO add support on non-empty and nullable collections
    }else {
      new Field(FieldType(symbol.typeSignature), if(symbol.isParamWithDefault) FieldFlags.WithDefault else FieldFlags.Required)
    }
  }
}

sealed trait FieldType {}
object FieldType {
  def apply(t: SysType): FieldType = {
    if      (t =:= typeOf[String]) RudimentTypes.Text
    else if (t =:= typeOf[Int] || t =:= typeOf[Double] || t =:= typeOf[Long] || t =:= typeOf[BigDecimal]) RudimentTypes.Number
    else if (t =:= typeOf[Timestamp]) RudimentTypes.Timestamp
    else if (t =:= typeOf[Date]) RudimentTypes.Date
    else if (t =:= typeOf[Time]) RudimentTypes.Time
    //TODO add enum
    else RudimentTypes.Unknown // TODO add error handling
  }
}

trait DTO extends Product with FieldType
object RudimentTypes {
  case object Text      extends FieldType
  case object Number    extends FieldType
  case object Enum      extends FieldType

  case object Date      extends FieldType
  case object Time      extends FieldType
  case object Timestamp extends FieldType

  case object Unknown   extends FieldType
}

sealed trait FieldFlag {}
object FieldFlags {
  case object Required    extends FieldFlag
  case object Optional    extends FieldFlag
  case object WithDefault extends FieldFlag
}

sealed trait CollectionFlag extends FieldFlag {}
object CollectionFlags {
  case object NonEmpty    extends CollectionFlag
  case object CanBeEmpty  extends CollectionFlag
  case object Nullable    extends CollectionFlag
  case object WithDefault extends CollectionFlag
}