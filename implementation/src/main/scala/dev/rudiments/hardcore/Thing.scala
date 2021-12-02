package dev.rudiments.hardcore

import dev.rudiments.hardcore.ScalaTypes.plain
import dev.rudiments.hardcore.Size.Big

import scala.collection.immutable.ListMap
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.{Type => SysType, _}

sealed trait Thing {}

sealed trait Ref extends Thing {}
final case class ID(k: Any) extends Ref
final case class Data(p: Predicate, v: Any) extends Ref {
  def apply(cmd: Command): Event = ???
  def apply(evt: Event): Data = ???
}
object Data {
  def build[T : ClassTag : TypeTag](args: Any*): Data = {
    val t = Type.build[T]
    Data(t, Seq(args: _*))
  }

  def apply[T : ClassTag : TypeTag](value: T): Data = {
    val t = Type.fullName(typeOf[T].typeSymbol)
    plain.get(t) match {
      case Some(p) => new Data(p, value)
      case None => value match {
        case p: Product => build[T](p.productIterator.toList: _*)
      }
    }
  }
}
final case class List(item: Predicate) extends Thing
final case class Index(of: Predicate, over: Predicate) extends Thing

class Instruction(f: Any => Any) extends Thing {}
sealed trait Expression extends Thing {}
sealed trait Predicate extends Expression {}
case class Skill(act: PartialFunction[In, Out], commit: PartialFunction[Out, Data]) extends Expression {}

final case class Abstract(fields: ListMap[String, Predicate]) extends Predicate
final case class Type(fields: ListMap[String, Predicate]) extends Predicate

object Type {
  def build[A : TypeTag]: Predicate = make(typeOf[A])

  def make(sysType: SysType): Predicate = {
    val symbol = sysType.typeSymbol
    val name = this.name(symbol)

    plain.getOrElse(this.fullName(symbol), if (sysType <:< typeOf[Product]) {
      makeAlgebraic(symbol)
    } else {
      throw new IllegalArgumentException(s"Scala type not supported: $name")
    })
  }

  def makeAlgebraic(symbol: Symbol): Predicate = {
    val t = symbol.asType
    if(t.isAbstract) {
      Abstract(fieldsOf(t))
    } else if(t.isModuleClass) {
      ??? // TODO singletone as ID -> Data, but Data is not predicate
    } else if(t.isClass) {
      Type(fieldsOf(t))
    } else {
      throw new IllegalArgumentException(s"Scala type ${t.name} not algebraic")
    }
  }

  def fieldsOf(t: TypeSymbol): ListMap[String, Predicate] = {
    val paramLists = t.asClass.primaryConstructor.typeSignature.paramLists
    if(paramLists.isEmpty) {
      ListMap.empty
    } else {
      ListMap(paramLists.head.collect { case ts: TermSymbol => this.name(ts) -> fieldOf(ts) }: _*)
    }
  }

  def fieldOf(ts: TermSymbol): Predicate = {
    if(ts.typeSignature <:< typeOf[Option[_]]) {
      make(ts.typeSignature.typeArgs.head)
    } else {
      AllOf(make(ts.typeSignature), Required)
    }
  }

  def name(s: Symbol): String = s.name.toString.trim
  def fullName(s: Symbol): String = s.fullName.trim
}

sealed trait Plain extends Predicate {}

object Plain {
  case object Bool extends Plain

  case class Text (
    maxSize: Size
  ) extends Plain

  case class Number (
    min: Size,
    max: Size,
    format: NumberFormat
  ) extends Plain

  sealed trait Temporal extends Plain {}
  case object Date extends Temporal
  case object Time extends Temporal
  case object Timestamp extends Temporal

  case object UUID extends Plain
}


sealed trait Size
object Size {
  case class  Big(size: BigDecimal) extends Size
  case object Infinity              extends Size
  case object PositiveInfinity      extends Size
  case object NegativeInfinity      extends Size
}

sealed trait NumberFormat
object NumberFormat {
  case object Integer extends NumberFormat
  case object Float   extends NumberFormat
  case object Decimal extends NumberFormat
}

sealed trait CompositePredicate extends Predicate {}
case class AllOf(p: Predicate*) extends CompositePredicate
case class AnyOf(p: Predicate*) extends CompositePredicate
case class OneOf(p: Predicate*) extends CompositePredicate

case object Required extends Predicate {} //TODO check various cases