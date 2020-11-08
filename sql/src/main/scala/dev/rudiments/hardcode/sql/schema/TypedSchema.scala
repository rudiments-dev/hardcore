package dev.rudiments.hardcode.sql.schema

import dev.rudiments.domain.{DTO, Spec}

case class TypedSchema(
  name: String,
  tables: Map[Spec, Table],
  references: Set[FK]
) extends DTO

case class Table(
  name: String,
  columns: Seq[Column]
) extends DTO {
  val pk: Seq[Column] = columns.filter(_.pk)
}

case class Column(
  name: String,
  `type`: ColumnType,
  nullable: Boolean,
  default: Boolean,
  pk: Boolean
) extends DTO

case class FK(
  from: Table,
  to: Table,
  references: Map[Column, Column]
) extends DTO {
  override def toString: String = {
    val refs = references.toSeq
    from.name + "(" + refs.map(_._1).mkString(", ") + ") -> " + to.name + refs.map(_._2).mkString(", ") + ")"
  }
}