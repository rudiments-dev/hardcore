package dev.rudiments.another

import dev.rudiments.another.ScalaTypes._
import enumeratum.EnumEntry

import scala.collection.immutable.ListMap
import scala.reflect.ClassTag

trait DTO extends Product
trait ADT extends Product

sealed trait Ref extends ADT
case class Instance(spec: Spec, values: Seq[Any]) extends Ref {
  def extract[T : ClassTag](field: String): T = spec.fields.zip(values).collectFirst {
    case ((n, _), v: T) if n == field => v
  }.getOrElse {
    throw new IllegalArgumentException(s"Field $field not found")
  }
}
case class ID(values: Seq[Any]) extends Ref

sealed abstract class Thing (
  val name: String
) extends ADT {
  def validate(system: TypeSystem, value: Any): Any
}

case class ThingRef (
  override val name: String
) extends Thing(name) {
  override def validate(system: TypeSystem, value: Any): Any = system.types.get(name) match {
    case Some(t: Thing) => t.validate(system, value)
    case None => throw new RuntimeException(s"Not found by Ref: $name")
  }
}

case class Abstract (
  override val name: String
) extends Thing(name) {
  override def validate(system: TypeSystem, value: Any): Any = value match {
    case i: Instance => system.afterParent(this, i.spec.name).validate(system, i)
    case one: The => system.afterParent(this, one.name).validate(system, one)
    case t: Thing if name == "Thing" => t
    case other => throw new IllegalArgumentException(s"Incompatible $other with Abstract $name")
  }
}

case class The (
  override val name: String
  //TODO value: Instance = Nothing
) extends Thing(name) {
  override def validate(system: TypeSystem, value: Any): Any = value match {
    case one: The if name == one.name => one
  }
}

case class Spec (
  override val name: String,
               fields: ListMap[String, ValueSpec]
) extends Thing(name) {

  override def validate(system: TypeSystem, value: Any): Any = value match {
    case i: Instance =>
      if(i.spec == this) {
        fields.zip(i.values).map { case ((_, spec), v) => spec.validate(system, v) }
        i
      } else {
        throw new IllegalArgumentException(s"Incompatible instance of ${i.spec.name} with $name")
      }
    case other => throw new IllegalArgumentException(s"Not an Instance: $other of $name")
  }

  def instantiate(system: TypeSystem, args: Any*): Instance = {
    Instance(
      this,
      fields.zip(args).map {
        case ((_, spec), value) => spec.validate(system, value)
      }.toSeq
    )
  }

  def fromMap(system: TypeSystem, args: Map[String, Any]): Instance = {
    Instance(
      this,
      fields.map {
        case (name, spec) => spec.validate(system, args(name))
      }.toSeq
    )
  }

  def fromProduct(system: TypeSystem, product: Product): Instance = {
    val values = fields.zip(product.productIterator.toSeq).map {
      case ((_, spec), value: Option[_]) if !spec.isRequired =>
        spec.validate(system, value.map(i => wrap(system, spec.thing, i)))
      case ((_, spec), value) =>
        spec.validate(system, wrap(system, spec.thing, value))
    }.toSeq
    Instance(this, values)
  }

  def wrap(system: TypeSystem, thing: Thing, value: Any): Any = (thing, value) match {
    case (p: Plain, v) => wrapPlain(p, v)

    case (Index(of, over), m: Map[_, _]) => m.map { case (k, v) => wrap(system, of, k) -> wrap(system, over, v)}
    case (List(of), i: Iterable[_]) => i.map { v => wrap(system, of, v) }

    case (spec: Spec, i: Instance) if spec == i.spec => i
    case (spec: Spec, product: Product) => spec.fromProduct(system, product)

    case (a: Abstract, i: Instance) =>
      system.afterParent(a, i.spec.name) match {
        case spec: Spec if spec == i.spec => i
        case other => throw new IllegalArgumentException(s"Incompatible $i: ADT with thing $other")
      }
    case (a: Abstract, adt: ADT) =>
      system.afterParent(a, adt.productPrefix) match {
        case spec: Spec => spec.fromProduct(system, adt)
        case one: The => one
        case other => throw new IllegalArgumentException(s"Incompatible ${adt.productPrefix}: ADT with thing $other")
      }
    case (a: Abstract, enum: EnumEntry) =>
      system.afterParent(a, enum.entryName) match {
        case one: The => one
        case other => throw new IllegalArgumentException(s"Incompatible enum ${other} with thing $other")
      }


    case (ThingRef(r), any) => wrap(system, system.find(r), any)

    case (t, v) => throw new IllegalArgumentException(s"Incompatible ${t.name} with value $v")
  }

  import scala.reflect.runtime.universe._
  def toScala[T : TypeTag](system: TypeSystem, args: Any*): T = {
    val c = Class.forName(typeOf[T].typeSymbol.asClass.fullName)
    c.getConstructors()(0).newInstance(args.map(_.asInstanceOf[Object]): _*).asInstanceOf[T]
  }
}

object Anything extends The("Anything")

sealed abstract class Plain (
  override val name: String
) extends Thing(name) {
  override def validate(system: TypeSystem, value: Any): Any = value
}

object Plain {
  case object Bool extends Plain("Bool")

  case class Text (
    maxSize: Size
  ) extends Plain("Text")

  case class Number (
    min: Size,
    max: Size,
    format: NumberFormat
  ) extends Plain("Number")

  case object Date extends Plain("Date")
  case object Time extends Plain("Time")
  case object Timestamp extends Plain("Timestamp")

  case object UUID extends Plain("UUID")
}

case class List (
  of: Thing
) extends Thing("List") {
  override def validate(system: TypeSystem, value: Any): Any = value match {
    case i: Iterable[_] => i.map(v => of.validate(system, v))
    case other => throw new IllegalArgumentException(s"$other is not Iterable")
  }
}

case class Index (
  of: Thing,
  over: Thing
) extends Thing("Index") {
  override def validate(system: TypeSystem, value: Any): Any = value match {
    case m: Map[_, _] => m.map(v => of.validate(system, v._1) -> over.validate(system, v._2))
    case other => throw new IllegalArgumentException(s"$other is not Map")
  }
}


case class ValueSpec (
  thing: Thing,
  isRequired: Boolean
) extends DTO {
  def validate(system: TypeSystem, value: Any): Any = value match {
    case other if isRequired => thing.validate(system, value)
    case o: Option[_] if !isRequired => o.map(v => thing.validate(system, v))
  }
}

sealed trait Size extends ADT
object Size {
  case class  Big(size: BigDecimal) extends Size
  case object Infinity              extends Size
  case object PositiveInfinity      extends Size
  case object NegativeInfinity      extends Size
}

sealed trait NumberFormat extends ADT
object NumberFormat {
  case object Integer extends NumberFormat
  case object Float   extends NumberFormat
  case object Decimal extends NumberFormat
}