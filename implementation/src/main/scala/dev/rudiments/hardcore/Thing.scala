package dev.rudiments.hardcore

import dev.rudiments.hardcore.ScalaTypes._

import scala.reflect.runtime.universe.{Type => SysType, _}

sealed trait Thing {}

sealed trait Ref extends Thing {}
final case class ID(k: Any) extends Ref
final case class Data(p: Predicate, v: Any) extends Ref {
  def apply(cmd: Command): Event = ???
  def apply(evt: Event): Data = ???

  def reconstruct[T](): T = {
    p match {
      case _: Plain => v.asInstanceOf[T] // validate?
      case Type(_, Some(fullName)) =>
        Class.forName(fullName).getConstructors()(0)
          .newInstance(v.asInstanceOf[Seq[Object]]: _*).asInstanceOf[T]
      case other => throw new IllegalArgumentException(s"Can't reconstruct from $other")
    }

  }
}
object Data {
  def build[T : TypeTag](args: Any*): Data = {
    val t = Type.build[T]
    Data(t, Seq(args: _*))
  }

  def apply[T : TypeTag](value: T): Data = value match {
    case s: String => Data(Plain.Text(s.size), s)
    case i: Int => Data(ScalaInt, i)
    case i: Long => Data(ScalaLong, i)
    case i: BigInt => Data(ScalaBigInteger, i)
    case i: BigDecimal => Data(ScalaBigDecimal, i) //TODO think about level of construction between Data and Type

    case p: Product => build[T](p.productIterator.toList: _*)
  }
}
final case class List(item: Predicate) extends Thing
final case class Index(of: Predicate, over: Predicate) extends Thing

class Instruction(f: Any => Any) extends Thing {}
sealed trait Expression extends Thing {}
sealed trait Predicate extends Expression {}
case class Skill(act: PartialFunction[In, Out], commit: PartialFunction[Out, Data]) extends Expression {}

final case class Abstract(fields: Seq[Field] = Seq.empty) extends Predicate
final case class Type(fields: Seq[Field] = Seq.empty, fullName: Option[String] = None) extends Predicate

object Type {
  def build[A : TypeTag]: Predicate = make(typeOf[A])

  def make(sysType: SysType): Predicate = {
    val symbol = sysType.typeSymbol
    val name = this.name(symbol)

    plain.getOrElse(this.fullName(symbol), if (sysType <:< typeOf[Any]) {
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
      AllOf(fieldsOf(t): _*)
    } else if(t.isClass) {
      Type(fieldsOf(t), Some(fullName(t)))
    } else {
      throw new IllegalArgumentException(s"Scala type ${t.name} not algebraic")
    }
  }

  def fieldsOf(t: TypeSymbol): Seq[Field] = {
    val paramLists = t.asClass.primaryConstructor.typeSignature.paramLists
    if(paramLists.isEmpty) {
      Seq.empty
    } else {
      Seq(paramLists.head.collect { case ts: TermSymbol => Field(this.name(ts), ifOption(ts)) }: _*)
    }
  }

  def ifOption(ts: TermSymbol): Predicate = {
    if(ts.typeSignature <:< typeOf[Option[_]]) {
      make(ts.typeSignature.typeArgs.head)
    } else {
      AllOf(make(ts.typeSignature), Required)
    }
  }

  def name(s: Symbol): String = s.name.toString.trim
  def fullName(s: Symbol): String = s.fullName.trim
}

case class Field(name: String, p: Predicate) extends Predicate

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