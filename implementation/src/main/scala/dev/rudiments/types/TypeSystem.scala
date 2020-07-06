package dev.rudiments.types

import java.sql.{Date, Time, Timestamp}
import java.util.UUID

import dev.rudiments.types.hard.{ScalaType, ScalaTypes}
import enumeratum._

import scala.collection.immutable.ListMap
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.{Type => SysType}
import scala.collection.mutable

case class TypeSystem(types: mutable.Map[String, Thing] = mutable.Map.empty) extends DTO {
  def find(name: String): Option[Thing] = {
    types.get(name)
  }

  def save(t: Thing): Thing = {
    types += t.name -> t
    t
  }

  def getAbstract(name: String): Abstract = find(name) match {
    case Some(a: Abstract) => a
    case Some(a: Algebraic) => a.root
    case Some(other) => throw new RuntimeException(s"$name is not an Abstract, it's $other")
    case None => throw new ThingNotFound(name)
  }

  def getType(name: String): Type = find(name) match {
    case Some(t: Type) => t
    case Some(other) => throw new RuntimeException(s"$name is not a Type, it's $other")
    case None => throw new ThingNotFound(name)
  }

  def getOnlyOne(name: String): OnlyOne = find(name) match {
    case Some(o: OnlyOne) => o
    case Some(other) => throw new RuntimeException(s"$name is not OnlyOne, it's $other")
    case None => throw new ThingNotFound(name)
  }

  def getAlgebraic(name: String): Algebraic = find(name) match {
    case Some(a: Algebraic) => a
    case Some(other) => throw new RuntimeException(s"$name is not an Algebraic, it's $other")
    case None => throw new ThingNotFound(name)
  }

  def descendants(t: Thing): Set[Thing] = t match {
    case a: Algebraic => a.descendants
    case a: Abstract => types.values.collect { case i if i.ascendants.contains(a) => i }.toSet
    case _ => Set.empty
  }

  def asType[T : TypeTag]: ScalaType[T] = asType(typeOf[T].typeSymbol, Seq.empty)

  def asType[T : TypeTag](t: Symbol, asc: Seq[Thing]): ScalaType[T] = {
    try {
      new ScalaType[T](
        t.name.toString.trim,
        castFields(t),
        asc
      )
    } catch {
      case e: Exception => throw new RuntimeException(s"Failed to instantiate Scala Type for ${t.fullName.trim}", e)
    }
  }

  def castFields(t: Symbol): Map[String, Field] =
    ListMap(t.asClass.primaryConstructor.typeSignature.paramLists.head.collect {
      case ts: TermSymbol => name(ts) -> fieldOf(ts)
    }: _*)

  def fieldOf(ts: TermSymbol): Field = {
    if(ts.typeSignature <:< typeOf[Option[_]]) {
      Field(name(ts), thingOf(ts.typeSignature.typeArgs.head), isRequired = false)
    } else {
      Field(name(ts), thingOf(ts.typeSignature), isRequired = !ts.isParamWithDefault) //TODO implement default as Instance
    }
  }
  
  private val plain: Map[SysType, Plain] = Map(
    typeOf[Boolean] ->    Plain.Bool,
    typeOf[String] ->     ScalaTypes.ScalaString,
    typeOf[UUID] ->       Plain.UUID,

    typeOf[Byte] ->       ScalaTypes.ScalaByte,
    typeOf[Short] ->      ScalaTypes.ScalaShort,
    typeOf[Int] ->        ScalaTypes.ScalaInt,
    typeOf[Long] ->       ScalaTypes.ScalaLong,
    typeOf[Float] ->      ScalaTypes.ScalaFloat,
    typeOf[Double] ->     ScalaTypes.ScalaDouble,

    typeOf[BigInt] ->     ScalaTypes.ScalaBigInteger,
    typeOf[BigDecimal] -> ScalaTypes.ScalaBigDecimal,

    typeOf[Date] ->       Plain.Date,
    typeOf[Time] ->       Plain.Time,
    typeOf[Timestamp] ->  Plain.Timestamp
  )

  def asThing[T : TypeTag]: Thing = thingOf(typeOf[T])

  def thingOf(t: SysType, asc: Seq[Thing] = Seq.empty): Thing = {
    plain.collectFirst {
      case (k, v) if t =:= k => v
    }.getOrElse {
      if (t <:< typeOf[EnumEntry]) {
        val ru = runtimeMirror(getClass.getClassLoader)
        val companion = ru.reflectModule(ru.staticModule(t.toString)).instance.asInstanceOf[enumeratum.Enum[_ <: EnumEntry]]
        val root = Abstract(name(t.typeSymbol))
        save(root)
        companion.values.map(v => OnlyOne(v.entryName, Seq(root))).foreach(save)
        save(Algebraic(root, descendants(root).toSet))
      }
      else if (t <:< typeOf[Map[_, _]]) {
        Index(thingOf(t.typeArgs.head, asc), thingOf(t.typeArgs.last, asc))
      }
      else if (t <:< typeOf[Iterable[_]]) {
        List(thingOf(t.typeArgs.head, asc))
      }
      else if (t <:< typeOf[DTO]) {
        this.find(name(t.typeSymbol)) match {
          case Some(thing) => thing
          case None => save(this.asType(t.typeSymbol, asc))
        }
      }
      else if (t <:< typeOf[ADT]) {
        this.find(name(t.typeSymbol)) match {
          case Some(thing) => thing
          case None => forAlgebraic(t.typeSymbol, Seq.empty)
        }
      }
      else if (t =:= typeOf[Any]) {
        Anything
      }
      else {
        println("is ADT: " + (t <:< typeOf[ADT]))
        throw new ScalaTypeNotSupported(t)
      }
    }
  }

  def forAlgebraic(s: Symbol, asc: Seq[Thing]): Thing = {
    val t = s.asType
    if(t.isAbstract) {
      val a = Abstract(name(t))
      save(a)
      t.asClass.knownDirectSubclasses.map(s => forAlgebraic(s, asc.+:(a))).foreach(save)
      save(Algebraic(a, descendants(a).toSet, asc))
    } else if(t.isModuleClass) {
      OnlyOne(name(t), asc)
    } else if(t.isClass) {
      this.asType[Any](t, asc)
    } else {
      throw new RuntimeException(s"Scala type ${fullName(s)} non compatible with Algebraic")
    }
  }

  private def name(s: Symbol): String = s.name.toString.trim
  private def fullName(s: Symbol): String = s.fullName.trim
}

class ScalaTypeNotSupported(t: SysType) extends RuntimeException(s"Scala type ${t.typeSymbol.fullName} not supported")
class ThingNotFound(name: String) extends RuntimeException(s"Thing $name not found")