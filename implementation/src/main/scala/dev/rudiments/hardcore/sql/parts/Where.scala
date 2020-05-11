package dev.rudiments.hardcore.sql.parts

import dev.rudiments.hardcore.sql.schema.Column

case class Where(expressions: Set[WhereExpression])

sealed trait WhereExpression
case class ColumnWhereExpression(column: Column, predicate: SqlPredicate) extends WhereExpression