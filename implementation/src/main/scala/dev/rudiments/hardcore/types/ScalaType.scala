package dev.rudiments.hardcore.types

import java.sql.{Date, Time, Timestamp}
import java.util.UUID

import enumeratum._

import scala.collection.immutable.ListMap
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.{Type => SysType}

class ScalaType[T : TypeTag] (
  name: String,
  fields: Map[String, Field]
) extends Type(name, fields) {
  def constructScala(arguments: Any*): T = {
    val c = Class.forName(typeOf[T].typeSymbol.asClass.fullName)
    c.getConstructors()(0).newInstance(arguments.map(_.asInstanceOf[Object]): _*).asInstanceOf[T]
  }

  def extract(value: T, field: String): Any = value match {
    case v: Product => productField(v, field)
    case other      => ???
  }

  def productField(p: Product, f: String): Any = {
    fields
      .zipWithIndex
      .filter { case ((n, _), _) => n == f }
      .collectFirst { case ((_, _), i) => p.productElement(i) }
      .get
  }
}

object ScalaType {
  def apply[T : TypeTag](implicit typeSystem: TypeSystem): ScalaType[T] = apply(typeOf[T].typeSymbol)

  def apply[T : TypeTag](t: Symbol)(implicit typeSystem: TypeSystem): ScalaType[T] = {
    try {
      new ScalaType[T](
        t.name.toString.trim,
        collect(t)
      )
    } catch {
      case e: Exception => throw new RuntimeException(s"Failed to instantiate Scala Type for ${t.fullName.trim}", e)
    }

  }

  def collect(t: Symbol)(implicit typeSystem: TypeSystem): Map[String, Field] =
    ListMap(t.asClass.primaryConstructor.typeSignature.paramLists.head.collect {
      case m: TermSymbol => m.name.toString.trim -> ScalaField(m)
    }: _*)
}

object ScalaDeclaration {
  def apply[T : TypeTag]: Declaration = apply(typeOf[T].typeSymbol)
  def apply(t: Symbol): Declaration = {
    Declaration(t.name.toString.trim)
  }
}

object ScalaField {
  def apply(symbol: TermSymbol)(implicit typeSystem: TypeSystem): Field = {
    if(symbol.typeSignature <:< typeOf[Option[_]]) {
      Field(apply(symbol.typeSignature.typeArgs.head), FieldFlag.Optional)
    } else if(symbol.typeSignature <:< typeOf[Iterable[_]]) {
      Field(apply(symbol.typeSignature), if(symbol.isParamWithDefault) FieldFlag.CanBeEmpty else FieldFlag.NonEmpty)
      //TODO add support on non-empty and nullable collections
    }else {
      Field(apply(symbol.typeSignature), if(symbol.isParamWithDefault) FieldFlag.WithDefault else FieldFlag.Required)
    }
  }

  def apply(t: SysType)(implicit typeSystem: TypeSystem): FieldType = {
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
      val companion = ru.reflectModule(ru.staticModule(t.toString)).instance.asInstanceOf[enumeratum.Enum[_ <: EnumEntry]]
      Types.Reference(
        new Enum(
          t.toString,
          ScalaDeclaration(t.typeSymbol),
          companion.values.map(v => Singleton(v.entryName))
        )
      )
    }
    else if (t <:< typeOf[Map[_, _]]) {
      Types.Index(apply(t.typeArgs.head), apply(t.typeArgs.last))
    }
    else if (t <:< typeOf[Iterable[_]]) {
      Types.List(apply(t.typeArgs.head))
    }
    else if (t <:< typeOf[DTO]) {
      Types.Reference(typeSystem.find(t.typeSymbol.name.toString.trim) match {
        case Some(thing) => thing
        case None => typeSystem.save(ScalaType(t.typeSymbol))
      })
    }
    else if (t <:< typeOf[ADT]) {
      Types.Reference(typeSystem.find(t.typeSymbol.name.toString.trim) match {
        case Some(thing) => thing
        case None =>
          typeSystem.save(LateInit(t.typeSymbol.name.toString.trim))
          typeSystem.save(forAlgebraic(t.typeSymbol))
      })
    }
    else Types.Unknown // TODO add error handling
  }

  def forAlgebraic(s: Symbol)(implicit typeSystem: TypeSystem): Thing = {
    val t = s.asType
    if(t.isAbstract) {
      Algebraic(
        t.name.toString.trim,
        Declaration(t.name.toString.trim),
        t.asClass.knownDirectSubclasses.map(s => forAlgebraic(s)).toSeq
      )
    } else if(t.isModuleClass) {
      Singleton(t.name.toString.trim)
    } else if(t.isClass) {
      ScalaType[Any](t)
    } else {
      throw new RuntimeException(s"Symbol $s non compatible with Algebraic")
    }
  }
}