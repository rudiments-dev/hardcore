package dev.rudiments.hardcore

import scala.reflect.ClassTag

sealed trait Thing {}

sealed trait Ref extends Thing {}
final case class ID(k: Any) extends Ref
final case class Data(p: Predicate, v: Any) extends Ref {
  def apply(cmd: Command): Event = ???
  def apply(evt: Event): Data = ???
}
object Data {
  def build[T : ClassTag](args: Any*): Data = {
    val t = Type.build[T]
    Data(t, Seq(args: _*))
  }

  def apply[T : ClassTag](value: T): Data = value match {
    case p: Product => build[T](p.productIterator.toList: _*)
  }
}
final case class List(item: Predicate) extends Thing
final case class Index(of: Predicate, over: Predicate) extends Thing

class Instruction(f: Any => Any) extends Thing {}
sealed trait Expression extends Thing {}
sealed trait Pure extends Expression {}
sealed trait Predicate extends Pure {}
abstract class Skill[Ctx](in: Predicate, ctx: Ctx, out: Predicate) extends Expression {}

final case class Abstract(fields: Map[String, Predicate]) extends Predicate
final case class Type(fields: Map[String, Predicate]) extends Predicate
object Type {
  def build[T : ClassTag]: Type = ???
}

sealed trait Plain extends Predicate {}
final case class Number(size: Size, format: NumberFormat) extends Plain
final case class Text(size: Size) extends Plain
case object UUID extends Plain
sealed trait Temporal extends Plain {}
case object Date extends Temporal
case object Time extends Temporal
case object Timestamp extends Temporal


sealed trait NumberFormat {}
case object Integer extends NumberFormat
case object Float extends NumberFormat
case object Decimal extends NumberFormat

sealed trait Size {}
final case class Big(size: BigInt) extends Size
case object PositiveInfinity extends Size
case object NegativeInfinity extends Size