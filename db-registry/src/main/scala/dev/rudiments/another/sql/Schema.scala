package dev.rudiments.another.sql

import dev.rudiments.another.hardcore.{ID, State}

case class Schema(
  name: String,
  tables: State[Table] = new State,
  references: State[FK] = new State
)

case class Table(
  name: String,
  columns: Seq[Column]
) {
  val pk: Seq[Column] = columns.filter(_.pk)
}

case class Column(
  name: String,
  `type`: ColumnType,
  nullable: Boolean,
  default: Boolean,
  pk: Boolean
)

case class FK(
  name: String,
  from: TableRef,
  to: TableRef
)

case class TableRef(
  table: ID[Table],
  columns: Seq[ID[Column]]
)