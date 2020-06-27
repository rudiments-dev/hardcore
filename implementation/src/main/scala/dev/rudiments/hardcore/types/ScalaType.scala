package dev.rudiments.hardcore.types

import java.sql.{Date, Time, Timestamp}
import java.util.UUID

import enumeratum._

import scala.collection.immutable.ListMap
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.{Type => SysType}

class ScalaType[T : TypeTag] (
  override val name: String,
  override val fields: Map[String, Field]
) extends Type(name, fields) {
  def constructScala(arguments: Any*): T = {
    val c = Class.forName(typeOf[T].typeSymbol.asClass.fullName)
    c.getConstructors()(0).newInstance(arguments.map(_.asInstanceOf[Object]): _*).asInstanceOf[T]
  }

  def extract(value: T, field: String): Any = value match {
    case v: Product => productField(v, field)
    case other      => ???
  }
}

object ScalaType {
  def apply[T : TypeTag]: ScalaType[T] = apply(typeOf[T].typeSymbol)

  def apply[T : TypeTag](t: Symbol): ScalaType[T] = {
    new ScalaType[T](
      t.name.toString.trim,
      collect(t)
    )
  }

  def collect(t: Symbol): Map[String, Field] =
    ListMap(t.asClass.primaryConstructor.typeSignature.paramLists.head.collect {
      case m: TermSymbol => m.name.toString.trim -> ScalaField(m)
    }: _*)
}

object ScalaField {
  def apply(symbol: TermSymbol): Field = {
    if(symbol.typeSignature <:< typeOf[Option[_]]) {
      new Field(ScalaFieldType(symbol.typeSignature.typeArgs.head), FieldFlag.Optional)
    } else if(symbol.typeSignature <:< typeOf[Iterable[_]]) {
      new Field(ScalaFieldType(symbol.typeSignature), if(symbol.isParamWithDefault) FieldFlag.CanBeEmpty else FieldFlag.NonEmpty)
      //TODO add support on non-empty and nullable collections
    }else {
      new Field(ScalaFieldType(symbol.typeSignature), if(symbol.isParamWithDefault) FieldFlag.WithDefault else FieldFlag.Required)
    }
  }
}

object ScalaFieldType {
  def apply(t: SysType): FieldType = {
    if      (t =:= typeOf[Boolean])     Types.Bool

    else if (t =:= typeOf[String])      ScalaTypes.ScalaString

    else if (t =:= typeOf[Byte])        ScalaTypes.ScalaByte
    else if (t =:= typeOf[Short])       ScalaTypes.ScalaShort
    else if (t =:= typeOf[Int])         ScalaTypes.ScalaInt
    else if (t =:= typeOf[Long])        ScalaTypes.ScalaLong

    else if (t =:= typeOf[Float])       ScalaTypes.ScalaFloat
    else if (t =:= typeOf[Double])      ScalaTypes.ScalaDouble

    else if (t =:= typeOf[BigInt])      ScalaTypes.ScalaBigInteger
    else if (t =:= typeOf[BigDecimal])  ScalaTypes.ScalaBigDecimal

    else if (t =:= typeOf[Date])        Types.Date
    else if (t =:= typeOf[Time])        Types.Time
    else if (t =:= typeOf[Timestamp])   Types.Timestamp

    else if (t =:= typeOf[UUID])        Types.UUID

    else if (t <:< typeOf[EnumEntry]) {
      val ru = runtimeMirror(getClass.getClassLoader)
      val companion = ru.reflectModule(ru.staticModule(t.toString)).instance.asInstanceOf[Enum[_ <: EnumEntry]]
      Types.Enum(t.toString, companion.values.map(v => v.entryName))
    }
    else if (t <:< typeOf[Map[_, _]]) {
      Types.Index(ScalaFieldType(t.typeArgs.head), ScalaFieldType(t.typeArgs.last))
    }
    else if (t <:< typeOf[Iterable[_]]) {
      Types.List(ScalaFieldType(t.typeArgs.head))
    }
    else if (t <:< typeOf[DTO]) {
      Types.Reference(ScalaType(t.typeSymbol))
    }
    else Types.Unknown // TODO add error handling
  }
}