package dev.rudiments.hardcode.sql

import dev.rudiments.hardcode.sql.schema.{Column, TypedSchema, Table}

object SQLParts {

  case class From(schema: TypedSchema, table: Table) {
    def sql: String = s"${schema.name}.${table.name}"
  }

  case class SelectField(column: Column) {
    def sql: String = column.name
  }
  case class Select(selects: Seq[SelectField]) {
    def sql: String = selects.map { _.column.name }.mkString(", ")
  }
  case class Where(expressions: Set[ColumnPredicate])

  case class ColumnPredicate(column: Column, predicate: SQLPredicate)
}

case class SqlPart(sql: String, bindings: Seq[Binding] = Seq.empty) {
  def +(another: SqlPart): SqlPart = {
    SqlPart(
      sql + another.sql,
      bindings ++ another.bindings
    )
  }

  def ++(another: SqlPart*): SqlPart = {
    SqlPart(
      sql + another.map(_.sql).mkString,
      another.foldLeft(bindings)((acc, part) => acc ++ part.bindings)
    )
  }

  def and(another: SqlPart): SqlPart = {
    SqlPart(
      sql + " AND " + another.sql,
      bindings ++ another.bindings
    )
  }
}

case class Binding(key: String, value: Any) {
  def toScalike: (Symbol, Any) = Symbol(key) -> value
}
