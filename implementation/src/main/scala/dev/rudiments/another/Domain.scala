package dev.rudiments.another

import dev.rudiments.another.ScalaTypes.plain
import enumeratum.EnumEntry

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.reflect.ClassTag

case class Domain (
  model: mutable.Map[String, Thing] = mutable.Map.empty,
  groups: mutable.Map[String, Set[String]] = mutable.Map.empty
) extends DTO {
  def afterParent(a: Abstract, name: String): Thing = {
    model.get(name) match {
      case Some(spec: Spec) if groups(spec.name).contains(a.name) => spec
      case Some(one: The) if groups(one.name).contains(a.name) => one
      case Some(other) => throw new IllegalArgumentException(s"Incompatible $name with thing $other")
      case None => throw new RuntimeException(s"Not found: $name")
    }
  }

  def children(a: Abstract): Set[Thing] = model.collect {
    case (_, t) if groups(t.name).contains(a.name) => t
  }.toSet

  def children(name: String): Set[Thing] = model.collect {
    case (_, t) if groups(t.name).contains(name) => t
  }.toSet

  def find[T <: Thing : ClassTag](name: String): T = model.get(name) match {
    case Some(t: T) => t
    case Some(other) => throw new RuntimeException(s"Unexpected ${other.name}, not matched with $name")
    case None => throw new RuntimeException(s"Not found by name $name")
  }


  def save[T <: Thing](thing: T, parents: Set[String]): T = {
    model += thing.name -> thing
    this.groups += thing.name -> (
      this.groups.get(thing.name) match {
        case Some(existing) => existing ++ parents
        case None => parents
      }
      )
    thing
  }

  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.universe.{Type => SysType}

  def makeFromScala[T <: Thing, A : TypeTag]: T = make(typeOf[A], Set.empty).asInstanceOf[T]

  def make(sysType: SysType, parents: Set[String]): Thing = {
    val symbol = sysType.typeSymbol
    val name = this.name(symbol)

    model.get(name) match {
      case Some(t) => t
      case None =>
        plain.collectFirst {
          case (k, v) if sysType =:= k => v
        }.getOrElse {
          if (sysType <:< typeOf[EnumEntry]) {
            val ru = runtimeMirror(getClass.getClassLoader)
            val companion = ru.reflectModule(ru.staticModule(sysType.toString)).instance.asInstanceOf[enumeratum.Enum[_ <: EnumEntry]]
            val root = Abstract(name, fieldsOf(symbol.asType))
            val p = parents ++ Set(root.name)
            save(root, parents)
            companion.values.map(v => The(v.entryName)).foreach(t => save(t, p))
            root
          } else if (sysType <:< typeOf[Map[_, _]]) {
            Index(make(sysType.typeArgs.head, Set.empty), make(sysType.typeArgs.last, Set.empty))

          } else if (sysType <:< typeOf[Iterable[_]]) {
            List(make(sysType.typeArgs.head, Set.empty))

          } else if (sysType <:< typeOf[DTO]) {
            makeAlgebraic(symbol, parents)
          } else if (sysType <:< typeOf[ADT]) {
            makeAlgebraic(symbol, parents)

          } else if (sysType =:= typeOf[Any]) {
            Anything
          } else {
            throw new IllegalArgumentException(s"Scala type not supported: $name")
          }
        }
    }
  }

  def makeAlgebraic(symbol: Symbol, parents: Set[String]): Thing = {
    val t = symbol.asType
    val name = this.name(t)
    model.get(name) match {
      case Some(found) => found
      case None =>
        if(t.isAbstract) {
          val root = save(Abstract(name, fieldsOf(t)), parents)
          t.asClass.knownDirectSubclasses.map { s => makeAlgebraic(s, parents ++ Set(root.name)) }
          root
        } else if(t.isModuleClass) {
          save(The(name), parents)
        } else if(t.isClass) {
          save(Spec(name, fullName(t), fieldsOf(t)), parents)
        } else {
          throw new IllegalArgumentException(s"Scala type ${t.name} not algebraic")
        }
    }
  }

  def fieldsOf(t: TypeSymbol): ListMap[String, ValueSpec] = {
    val paramLists = t.asClass.primaryConstructor.typeSignature.paramLists
    if(paramLists.isEmpty) {
      ListMap.empty
    } else {
      ListMap(paramLists.head.collect { case ts: TermSymbol => this.name(ts) -> fieldOf(ts) }: _*)
    }
  }

  def fieldOf(ts: TermSymbol): ValueSpec = {
    if(ts.typeSignature <:< typeOf[Option[_]]) {
      ValueSpec(make(ts.typeSignature.typeArgs.head, Set.empty), isRequired = false)
    } else {
      ValueSpec(make(ts.typeSignature, Set.empty), isRequired = true) //TODO implement default as Instance
    }
  }

  private def name(s: Symbol): String = s.name.toString.trim
  private def fullName(s: Symbol): String = s.fullName.trim
}
