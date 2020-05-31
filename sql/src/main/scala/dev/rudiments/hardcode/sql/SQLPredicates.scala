package dev.rudiments.hardcode.sql

import dev.rudiments.hardcore.types.DTO

sealed trait SQLPredicate extends DTO

object SQLPredicates {

  case object IsNull extends SQLPredicate
  case object NotNull extends SQLPredicate
  case class Equals(value: Any) extends SQLPredicate
  case class NotEquals(value: Any) extends SQLPredicate
  case class In(in: Seq[Any]) extends SQLPredicate
  case class Greater(value: Any) extends SQLPredicate
  case class GreaterOrEquals(value: Any) extends SQLPredicate
  case class Lesser(value: Any) extends SQLPredicate
  case class LesserOrEquals(value: Any) extends SQLPredicate
  case class Between(from: Any, to: Any) extends SQLPredicate

}