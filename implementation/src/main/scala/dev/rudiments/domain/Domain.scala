package dev.rudiments.domain

import dev.rudiments.data._
import dev.rudiments.domain.ScalaTypes.plain
import dev.rudiments.hardcore.{Ask, Query, Reply, Skill}
import enumeratum.EnumEntry

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.reflect.ClassTag

class Domain extends Skill {
  private val model: mutable.Map[String, Thing] = mutable.Map.empty
  private val groups: mutable.Map[String, Set[String]] = mutable.Map.empty

  val state: State = new State

  makeFromScala[Thing, Thing]
  makeFromScala[Thing, Instance]
  makeFromScala[Thing, Type]

  val spec: Spec = find[Spec]("Spec")
  val the: Spec = find[Spec]("The")
  val abs: Spec = find[Spec]("Abstract")
  val t: Spec = find[Spec]("Type")
  val valueSpec: Spec = find[Spec]("ValueSpec")

  val skill: Skill = {
    case q: Query => state(q)

    case c@Create(ID(Seq(name: String)), _) =>
      state(c).flatMap[Created] { created =>
        if(created.value.spec == t) {
          val value = created.value.toScala[Type]
          save(name, value.thing, value.is.toSet)
          created
        } else {
          FailedToCreate(c.key, c.value)
        }
      }

    case c@Update(ID(Seq(name: String)), _) =>
      state(c).flatMap[Updated] { updated =>
        if(updated.newValue.spec == t) {
          val value = updated.newValue.toScala[Type]
          model.put(name, value.thing)
          groups.put(name, value.is.toSet)
          updated
        } else {
          FailedToUpdate(c.key, c.value)
        }
      }

    case c@Delete(ID(Seq(name: String))) =>
      state(c).also[Deleted] { _ =>
        model -= name
        groups -= name
      }
  }

  model.map { case (k, v) =>
    Create(
      ID(Seq(k)),
      t.fromProduct(this, Type(k, v, groups(k).toSeq))
    )
  }.foreach(skill.apply)


  override def apply(v1: Ask): Reply = skill.apply(v1)
  override def isDefinedAt(x: Ask): Boolean = skill.isDefinedAt(x)

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


  def save[T <: Thing](name: String, thing: T, parents: Set[String]): T = {
    assert(name == thing.name, "incoming name should be same with thing.name")
    model += name -> thing
    this.groups += name -> (
      this.groups.get(thing.name) match {
        case Some(existing) => existing ++ parents
        case None => parents
      }
    )
    thing
  }

  import scala.reflect.runtime.universe.{Type => SysType, _}

  def makeFromScala[T <: Thing, A : TypeTag]: T = make(typeOf[A], Set.empty).asInstanceOf[T]

  def make(sysType: SysType, parents: Set[String]): Thing = {
    val symbol = sysType.typeSymbol
    val name = this.name(symbol)

    plain.collectFirst {
      case (k, v) if sysType =:= k => v
    }.getOrElse {
      model.get(name) match {
        case Some(t) => t
        case None =>
          if (sysType <:< typeOf[EnumEntry]) {
            val ru = runtimeMirror(getClass.getClassLoader)
            val companion = ru.reflectModule(ru.staticModule(sysType.toString)).instance.asInstanceOf[enumeratum.Enum[_ <: EnumEntry]]
            val root = Abstract(name, fieldsOf(symbol.asType))
            val p = parents ++ Set(root.name)
            save(name, root, parents)
            companion.values.foreach { v => save(v.entryName, The(v.entryName), p) }
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
          val root = save(name, Abstract(name, fieldsOf(t)), parents)
          t.asClass.knownDirectSubclasses.map { s => makeAlgebraic(s, parents ++ Set(root.name)) }
          root
        } else if(t.isModuleClass) {
          save(name, The(name), parents)
        } else if(t.isClass) {
          save(name, Spec(name, fullName(t), fieldsOf(t)), parents)
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
