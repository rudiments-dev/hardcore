package dev.rudiments.another.sql

import dev.rudiments.another.hardcore.ID

case class Table(
  name: String,
  columns: Seq[Column],
  references: Set[FK]
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
  from: ID[Table],
  to: ID[Table],
  references: Map[ID[Column], ID[Column]]
) {
  override def toString: String = {
    val refs = references.toSeq
    from + "(" + refs.map(_._1).mkString(", ") + ") -> " + to + refs.map(_._2).mkString(", ") + ")"
  }
}