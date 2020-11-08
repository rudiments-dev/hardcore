package dev.rudiments.domain

import java.sql.{Date, Time, Timestamp}
import java.util.UUID

import dev.rudiments.domain.Plain.{Number, Text}
import dev.rudiments.domain.ScalaTypes._
import dev.rudiments.domain.Size.Big
import dev.rudiments.hardcore._
import enumeratum.EnumEntry

import scala.collection.immutable.ListMap
import scala.reflect.ClassTag

trait DTO extends Product
trait ADT extends Product

sealed trait Ref extends ADT
case class Instance(spec: Spec, values: Seq[Any]) extends Ref {
  override def toString: String = "[" + spec.name + values.mkString(":", ",", "]")

  def extract[T : ClassTag](field: String): T = spec.fields.zip(values).collectFirst {
    case ((n, _), v: T) if n == field => v
  }.getOrElse {
    throw new IllegalArgumentException(s"Field $field not found")
  }

  def copy[A : ClassTag](fieldName: String, f: A => Either[Error, A]): Either[Error, Instance] = this match {
    case s: Instance =>
      s.spec.fields.get(fieldName) match {
        case Some(field: A) =>
          f(field).map { ok =>
            Instance(s.spec, s.values) // Instance(s.fields + (fieldName -> ok))(s.t)
          }
        case None => Left(FieldNotFound(fieldName))
      }
    case other => ???
  }

  def matches(p: Predicate): Boolean = p match {
    case All => true

    case TypedPredicate(s, _) if spec != s => false //TODO inheritance, but requires Domain
    case TypedPredicate(s, w) if spec == s => w.forall(this.matches)

    case Equals(FieldExpression(f), ParameterExpression(value)) => this.extract[Any](f) == value
    case More(FieldExpression(f), ParameterExpression(value)) => this.extract[Comparable[Any]](f).compareTo(value) > 0
    case MoreOrEquals(FieldExpression(f), ParameterExpression(value)) => this.extract[Comparable[Any]](f).compareTo(value) >= 0
    case Less(FieldExpression(f), ParameterExpression(value)) => this.extract[Comparable[Any]](f).compareTo(value) < 0
    case LessOrEquals(FieldExpression(f), ParameterExpression(value)) => this.extract[Comparable[Any]](f).compareTo(value) <= 0

    case _ => ???
  }

  def toScala[T]: T = {
    spec.toScala[T](values: _*)
  }
}
case class ID(values: Seq[Any]) extends Ref {
  override def toString: String = values.mkString("#", "/", "")
}

sealed abstract class Thing (
  val name: String
) extends ADT {
  def validate(system: Domain, value: Any): Any
}

case class ThingRef (
  override val name: String
) extends Thing(name) {
  override def validate(system: Domain, value: Any): Any = ???
}

case class Abstract (
  override val name: String,
               fields: ListMap[String, ValueSpec]
) extends Thing(name) {
  override def validate(system: Domain, value: Any): Any = value match {
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
  override def validate(system: Domain, value: Any): Any = value match {
    case one: The if name == one.name => one
  }
}

case class Spec (
  override val name: String,
               fullName: String,
               fields: ListMap[String, ValueSpec]
) extends Thing(name) {

  override def validate(system: Domain, value: Any): Any = value match {
    case i: Instance =>
      if(i.spec == this) {
        fields.zip(i.values).map { case ((_, spec), v) => spec.validate(system, v) }
        i
      } else {
        throw new IllegalArgumentException(s"Incompatible instance of ${i.spec.name} with $name")
      }
    case other => throw new IllegalArgumentException(s"Not an Instance: $other of $name")
  }

  def instantiate(system: Domain, args: Any*): Instance = {
    Instance(
      this,
      fields.zip(args).map {
        case ((_, spec), value) => spec.validate(system, value)
      }.toSeq
    )
  }

  def fromMap(system: Domain, args: Map[String, Any]): Instance = {
    Instance(
      this,
      fields.map {
        case (name, spec) => spec.validate(system, args(name))
      }.toSeq
    )
  }

  def fromProduct(system: Domain, product: Product): Instance = {
    val values = fields.zip(product.productIterator.toSeq).map {
      case ((_, spec), value: Option[_]) if !spec.isRequired =>
        spec.validate(system, value.map(i => wrap(system, spec.thing, i)))
      case ((_, spec), value) =>
        spec.validate(system, wrap(system, spec.thing, value))
    }.toSeq
    Instance(this, values)
  }

  def wrap(system: Domain, thing: Thing, value: Any): Any = (thing, value) match {
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
    case (a: Abstract, adt: ADT) if a == adt => adt // ??? look at DomainRegistrySpec
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

  def toScala[T](args: Any*): T = {
    val unwrapped = args.collect {
      case the@The(name) => name match {
        case "Bool" => Plain.Bool
        case "Date" => Plain.Date
        case "Time" => Plain.Time
        case "Timestamp" => Plain.Timestamp
        case "UUID" => Plain.UUID

        case "Infinity" => Size.Infinity
        case "PositiveInfinity" => Size.PositiveInfinity
        case "NegativeInfinity" => Size.NegativeInfinity

        case "Integer" => NumberFormat.Integer
        case "Decimal" => NumberFormat.Decimal
        case "Float" => NumberFormat.Float

        case other => the
      }

      case i: Instance => i.toScala[Object]
      case m: Map[_, _] => m.map { case (k, v) => toScalaAsObject(k) -> toScalaAsObject(v) }
      case i: Iterable[_] => i.map(toScalaAsObject)
      case other => other.asInstanceOf[Object]
    }

    fullName match {
      case "dev.rudiments.domain.Spec" =>
        Spec( // nasty hack for case Map -> ListMap
          unwrapped(0).asInstanceOf[String],
          unwrapped(1).asInstanceOf[String],
          ListMap(unwrapped(2).asInstanceOf[Map[String, ValueSpec]].toSeq: _*)
        ).asInstanceOf[T]

      case "dev.rudiments.domain.Abstract" =>
        Abstract( // nasty hack for case Map -> ListMap
          unwrapped(0).asInstanceOf[String],
          ListMap(unwrapped(1).asInstanceOf[Map[String, ValueSpec]].toSeq: _*)
        ).asInstanceOf[T]


      case "dev.rudiments.domain.Size.Big" =>
        Big(unwrapped(0).asInstanceOf[BigDecimal]).asInstanceOf[T]

      case "dev.rudiments.domain.Plain.Text" =>
        Text(unwrapped(0).asInstanceOf[Size]).asInstanceOf[T]

      case "dev.rudiments.domain.Plain.Number" =>
        Number(
          unwrapped(0).asInstanceOf[Size],
          unwrapped(1).asInstanceOf[Size],
          unwrapped(2).asInstanceOf[NumberFormat]
        ).asInstanceOf[T]

      case _ =>
        Class.forName(fullName).getConstructors()(0).newInstance(unwrapped: _*).asInstanceOf[T]
    }
  }

  def toScalaAsObject(v: Any): Object = v match {
    case i: Instance => i.toScala[Object]
    case other => other.asInstanceOf[Object]
  }
}

object Anything extends The("Anything")

sealed abstract class Plain (
  override val name: String
) extends Thing(name) {
  override def validate(system: Domain, value: Any): Any = value
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
  override def validate(system: Domain, value: Any): Any = value match {
    case i: Iterable[_] => i.map(v => of.validate(system, v))
    case other => throw new IllegalArgumentException(s"$other is not Iterable")
  }
}

case class Index (
  of: Thing,
  over: Thing
) extends Thing("Index") {
  override def validate(system: Domain, value: Any): Any = value match {
    case m: Map[_, _] => m.map(v => of.validate(system, v._1) -> over.validate(system, v._2))
    case other => throw new IllegalArgumentException(s"$other is not Map")
  }
}


case class ValueSpec (
  thing: Thing,
  isRequired: Boolean
) extends DTO {
  def validate(system: Domain, value: Any): Any = value match {
    case other if isRequired => thing.validate(system, value)
    case o: Option[_] if !isRequired => o.map(v => thing.validate(system, v))
  }

  def parse(from: String): Any = if(thing.isInstanceOf[Plain]) {
    thing match {
      case Plain.Bool => from.toBoolean
      case Plain.Text(_) => from
      case Plain.Number(_, _, _) => BigDecimal(from)
      case Plain.Date => Date.valueOf(from)
      case Plain.Time => Time.valueOf(from)
      case Plain.Timestamp => Timestamp.valueOf(from)
      case Plain.UUID => UUID.fromString(from)
    }
  } else {
    ???
  }
}

case class Type (
  name: String,
  thing: Thing,
  is: Seq[String]
) extends DTO

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

case class FieldNotFound(field: String) extends Error