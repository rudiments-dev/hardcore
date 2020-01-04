package dev.rudiments.hardcore.types

import java.sql.{Date, Time, Timestamp}

import enumeratum._

import scala.collection.immutable.ListMap
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.{Type => SysType}

object TypeOps {
  def collect(t: Symbol): Map[String, Field] =
    ListMap(t.asClass.primaryConstructor.typeSignature.paramLists.head.collect {
      case m: TermSymbol => m.name.toString.trim -> Field(m)
    }: _*)
}

case class TypeSystem(name: String, types: Map[String, Type]) extends DTO
object TypeSystem {
  def apply(name: String, types: Type*): TypeSystem = new TypeSystem(name, types.map(t => t.name -> t).toMap)
}

case class Type(name: String, fields: Map[String, Field]) extends DTO
object Type {
  def apply[T <: DTO : TypeTag]: Type = apply(typeOf[T].typeSymbol)

  def apply(t: Symbol): Type = {
    new Type(
      t.name.toString.trim,
      TypeOps.collect(t)
    )
  }
}
class HardType[T <: DTO](name: String, fields: Map[String, Field]) extends Type(name, fields)
object HardType {
  def apply[T <: DTO : TypeTag]: HardType[T] = apply(typeOf[T].typeSymbol)

  def apply[T <: DTO](t: Symbol): HardType[T] = {
    new HardType[T](
      t.name.toString.trim,
      TypeOps.collect(t)
    )
  }
}

case class Field(kind: FieldType, fieldFlag: FieldFlag) extends DTO
object Field {
  def apply(symbol: TermSymbol): Field = {
    if(symbol.typeSignature <:< typeOf[Option[_]]) {
      new Field(FieldType(symbol.typeSignature.typeArgs.head), FieldFlags.Optional)
    } else if(symbol.typeSignature <:< typeOf[Iterable[_]]) {
      new Field(FieldType(symbol.typeSignature), if(symbol.isParamWithDefault) CollectionFlags.WithDefault else CollectionFlags.CanBeEmpty)
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
    else if (t =:= typeOf[Date]) RudimentTypes.Date
    else if (t =:= typeOf[Time]) RudimentTypes.Time
    else if (t =:= typeOf[Timestamp]) RudimentTypes.Timestamp
    else if (t <:< typeOf[EnumEntry]) {
      val ru = runtimeMirror(getClass.getClassLoader)
      val companion = ru.reflectModule(ru.staticModule(t.toString)).instance.asInstanceOf[Enum[_ <: EnumEntry]]
      RudimentTypes.Enum(t.toString, companion.values.map(v => v.entryName))
    }
    else if (t <:< typeOf[Set[_]]) {
      RudimentTypes.Set(FieldType(t.typeArgs.head))
    }
    else if (t <:< typeOf[Seq[_]]) {
      RudimentTypes.List(FieldType(t.typeArgs.head))
    }
    else if (t <:< typeOf[Map[_, _]]) {
      RudimentTypes.Index(FieldType(t.typeArgs.head), FieldType(t.typeArgs.last))
    }
    else if (t <:< typeOf[DTO]) {
      RudimentTypes.Reference(Type.apply(t.typeSymbol))
    }
    else RudimentTypes.Unknown // TODO add error handling
  }
}

trait DTO extends Product

object RudimentTypes {
  case object Text    extends FieldType
  case object Number  extends FieldType

  case object Date      extends FieldType
  case object Time      extends FieldType
  case object Timestamp extends FieldType

  case class Enum(name: String, values: Seq[String]) extends FieldType

  case class Set(of: FieldType) extends FieldType
  case class List(of: FieldType) extends FieldType
  case class Index(of: FieldType, over: FieldType) extends FieldType

  case class Reference(of: Type) extends FieldType

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