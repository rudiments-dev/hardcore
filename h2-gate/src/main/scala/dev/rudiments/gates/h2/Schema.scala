package dev.rudiments.gates.h2

import dev.rudiments.hardcore.{ADT, ID, Store}

case class Schema(name: String) extends ADT {
  val tables: Store[Table, Table] = new Store
  val references: Store[FK, FK] = new Store
}

case class Table(
  name: String,
  columns: Seq[Column]
) extends ADT {
  val pk: Seq[Column] = columns.filter(_.pk)
}

case class Column(
  name: String,
  `type`: ColumnType,
  nullable: Boolean,
  default: Boolean,
  pk: Boolean
) extends ADT

case class FK(
  name: String,
  from: TableRef,
  to: TableRef
) extends ADT

case class TableRef(
  table: ID[Table],
  columns: Seq[ID[Column]]
) extends ADT