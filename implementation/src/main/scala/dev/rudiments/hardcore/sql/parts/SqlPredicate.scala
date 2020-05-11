package dev.rudiments.hardcore.sql.parts

import dev.rudiments.hardcore.types.DTO

sealed trait SqlPredicate extends DTO
case object IsNull extends SqlPredicate
case object NotNull extends SqlPredicate
case class Equals(value: Any) extends SqlPredicate
case class NotEquals(value: Any) extends SqlPredicate
case class In(in: Seq[Any]) extends SqlPredicate
case class Greater(value: Any) extends SqlPredicate
case class GreaterOrEquals(value: Any) extends SqlPredicate
case class Lesser(value: Any) extends SqlPredicate
case class LesserOrEquals(value: Any) extends SqlPredicate
case class Between(from: Any, to: Any) extends SqlPredicate
