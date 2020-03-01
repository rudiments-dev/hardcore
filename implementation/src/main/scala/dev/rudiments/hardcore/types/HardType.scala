package dev.rudiments.hardcore.types

import java.sql.{Date, Time, Timestamp}

import enumeratum._

import scala.collection.immutable
import scala.collection.immutable.ListMap
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.{Type => SysType}

trait DTO extends Product

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

case class Type(name: String, fields: Map[String, Field]) extends DTO {
  def constructMap(arguments: Any*): Map[String, Any] = {
    fields.zip(arguments).map { case ((name, _), argument) =>
      name -> argument //TODO type check
    }
  }
}
object Type {
  def apply[T : TypeTag]: Type = apply(typeOf[T].typeSymbol)

  def apply(t: Symbol): Type = {
    new Type(
      t.name.toString.trim,
      TypeOps.collect(t)
    )
  }
}
class HardType[T : TypeTag](name: String, fields: Map[String, Field]) extends Type(name, fields) {
  def construct(arguments: Any*): T = {
    val c = Class.forName(typeOf[T].typeSymbol.asClass.fullName)
    c.getConstructors()(0).newInstance(arguments.map(_.asInstanceOf[Object]): _*).asInstanceOf[T]
  }
}
object HardType {
  def apply[T : TypeTag]: HardType[T] = apply(typeOf[T].typeSymbol)

  def apply[T : TypeTag](t: Symbol): HardType[T] = {
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
      new Field(FieldType(symbol.typeSignature.typeArgs.head), FieldFlag.Optional)
    } else if(symbol.typeSignature <:< typeOf[Iterable[_]]) {
      new Field(FieldType(symbol.typeSignature), if(symbol.isParamWithDefault) FieldFlag.CanBeEmpty else FieldFlag.NonEmpty)
      //TODO add support on non-empty and nullable collections
    }else {
      new Field(FieldType(symbol.typeSignature), if(symbol.isParamWithDefault) FieldFlag.WithDefault else FieldFlag.Required)
    }
  }
}

sealed trait FieldType {}
object FieldType {
  def apply(t: SysType): FieldType = {
    if      (t =:= typeOf[Boolean])     RudimentTypes.Bool

    else if (t =:= typeOf[String])      RudimentTypes.Text(Int.MaxValue)

    else if (t =:= typeOf[Byte])        RudimentTypes.Number.ScalaByte
    else if (t =:= typeOf[Short])       RudimentTypes.Number.ScalaShort
    else if (t =:= typeOf[Int])         RudimentTypes.Number.ScalaInt
    else if (t =:= typeOf[Long])        RudimentTypes.Number.ScalaLong

    else if (t =:= typeOf[Float])       RudimentTypes.Number.ScalaFloat
    else if (t =:= typeOf[Double])      RudimentTypes.Number.ScalaDouble

    else if (t =:= typeOf[BigInt])      RudimentTypes.Number.ScalaBigInteger
    else if (t =:= typeOf[BigDecimal])  RudimentTypes.Number.ScalaBigDecimal

    else if (t =:= typeOf[Date])        RudimentTypes.Date
    else if (t =:= typeOf[Time])        RudimentTypes.Time
    else if (t =:= typeOf[Timestamp])   RudimentTypes.Timestamp

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

object RudimentTypes {
  case object Bool extends FieldType
  case class Text(maxSize: Int)    extends FieldType
  object Text {
    object ScalaString extends Text(Int.MaxValue)
  }
  case class Number(min: NumberSize, max: NumberSize, format: NumberFormat)  extends FieldType
  object Number {
    object ScalaByte   extends Number(NumberSize.MinByte,  NumberSize.MaxByte,   NumberFormat.Integer)
    object ScalaShort  extends Number(NumberSize.MinShort, NumberSize.MaxShort,  NumberFormat.Integer)
    object ScalaInt    extends Number(NumberSize.MinInt,   NumberSize.MaxInt,    NumberFormat.Integer)
    object ScalaLong   extends Number(NumberSize.MinLong,  NumberSize.MaxLong,   NumberFormat.Integer)

    object ScalaFloat  extends Number(NumberSize.MinFloat,   NumberSize.MaxFloat,  NumberFormat.Float)
    object ScalaDouble extends Number(NumberSize.MinDouble,  NumberSize.MaxDouble, NumberFormat.Float)

    object ScalaBigInteger extends Number(NumberSize.NegativeInfinity, NumberSize.PositiveInfinity, NumberFormat.Integer)
    object ScalaBigDecimal extends Number(NumberSize.NegativeInfinity, NumberSize.PositiveInfinity, NumberFormat.Decimal)
  }

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

sealed trait NumberFormat extends EnumEntry
object NumberFormat extends Enum[NumberFormat] {
  override def values: immutable.IndexedSeq[NumberFormat] = findValues

  case object Integer extends NumberFormat
  case object Float   extends NumberFormat
  case object Decimal extends NumberFormat
}

sealed trait NumberSize
object NumberSize {
  case class Big(size: BigDecimal)  extends NumberSize

  case object Infinity              extends NumberSize
  case object PositiveInfinity      extends NumberSize
  case object NegativeInfinity      extends NumberSize

  object MinByte extends Big(BigDecimal(Byte.MinValue))
  object MaxByte extends Big(BigDecimal(Byte.MaxValue))

  object MinShort extends Big(BigDecimal(Short.MinValue))
  object MaxShort extends Big(BigDecimal(Short.MaxValue))

  object MinInt extends Big(BigDecimal(Int.MinValue))
  object MaxInt extends Big(BigDecimal(Int.MaxValue))

  object MinLong extends Big(BigDecimal(Long.MinValue))
  object MaxLong extends Big(BigDecimal(Long.MaxValue))

  object MinFloat extends Big(BigDecimal.decimal(Float.MinValue))
  object MaxFloat extends Big(BigDecimal.decimal(Float.MaxValue))

  object MinDouble extends Big(BigDecimal.decimal(Double.MinValue))
  object MaxDouble extends Big(BigDecimal.decimal(Double.MaxValue))
}

sealed trait FieldFlag extends EnumEntry
object FieldFlag extends Enum[FieldFlag] {
  override def values: immutable.IndexedSeq[FieldFlag] = findValues

  case object Required    extends FieldFlag
  case object Optional    extends FieldFlag
  case object WithDefault extends FieldFlag

  case object NonEmpty    extends FieldFlag
  case object CanBeEmpty  extends FieldFlag
}