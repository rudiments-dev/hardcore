package dev.rudiments.hardcode.sql

import dev.rudiments.hardcode.sql.schema.{Column, TypedSchema, Table}

object SQLParts {

  case class From(schema: TypedSchema, table: Table, as: Option[String])

  case class SelectField(column: Column, as: Option[String])
  case class Select(selects: Seq[SelectField])
  case class Where(expressions: Set[WhereExpression])

  sealed trait WhereExpression
  case class ColumnWhereExpression(column: Column, predicate: SQLPredicate) extends WhereExpression


}
