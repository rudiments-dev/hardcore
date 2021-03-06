package dev.rudiments.hardcore

import dev.rudiments.domain.Spec

sealed trait Predicate

case object All extends Predicate
case class Limited(limit: Int, offset: Long) extends Predicate

case class Equals(arg1: Expression, arg2: Expression) extends Predicate
case class More(arg1: Expression, arg2: Expression) extends Predicate
case class MoreOrEquals(arg1: Expression, arg2: Expression) extends Predicate
case class Less(arg1: Expression, arg2: Expression) extends Predicate
case class LessOrEquals(arg1: Expression, arg2: Expression) extends Predicate
case class Between(subj: Expression, from: Expression, to: Expression) extends Predicate

case class TypedPredicate(spec: Spec, where: Seq[Predicate]) extends Predicate
case class And(p1: Predicate, p2: Predicate) extends Predicate

case class Order(by: Sort*) extends Predicate

sealed trait SortDirection
case object Asc extends SortDirection
case object Desc extends SortDirection

case class Sort(field: String, direction: SortDirection = Asc)



sealed trait Expression
case class ConstExpression(value: Any) extends Expression
case class FieldExpression(name: String) extends Expression
case class ParameterExpression(value: Any) extends Expression