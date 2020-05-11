package dev.rudiments.hardcore.sql

import dev.rudiments.hardcore.sql.parts.{From, Select, Where}
import dev.rudiments.hardcore.sql.schema.{Column, Table}

sealed trait SQLDataClass

case class SelectSql
(
  select: Select,
  from: From,
  where: Where
) extends SQLDataClass

case class DeleteSql
(
  table: Table,
  where: Where
) extends SQLDataClass

case class InsertSql
(
  table: Table,
  entity: SqlEntity
) extends SQLDataClass


case class UpdateSql
(
  table: Table,
  entity: SqlEntity,
  where: Where
) extends SQLDataClass

case class SqlEntity(values: Seq[SqlValue])
case class SqlValue(column: Column, value: Any)