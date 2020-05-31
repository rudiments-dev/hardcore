package dev.rudiments.hardcode.sql

import dev.rudiments.hardcode.sql.schema.{Column, Schema, Table}

object SQLParts {

  case class From(schema: Schema, table: Table, as: Option[String])

  case class Selector(column: Column, as: Option[String])
  case class Select(selectors: Seq[Selector])
  case class Where(expressions: Set[WhereExpression])

  sealed trait WhereExpression
  case class ColumnWhereExpression(column: Column, predicate: SQLPredicate) extends WhereExpression


}
