package dev.rudiments.gates.h2

import dev.rudiments.hardcore.{ID, Store}

case class Schema(
  name: String,
  tables: Store[Table, Table] = new Store,
  references: Store[FK, FK] = new Store
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