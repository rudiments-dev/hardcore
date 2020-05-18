package dev.rudiments.hardcore.sql

import dev.rudiments.hardcore.sql.parts.{From, Select, Where}
import dev.rudiments.hardcore.sql.schema.{Column, Table}
import dev.rudiments.hardcore.types.{ID, Instance, Type}

sealed trait SQLDataClass {
  val softType: Type
}

case class FindByIdDataClass
(
  select: Select,
  from: From,
  where: Where,
  override val softType: Type,
  id: ID
) extends SQLDataClass

case class QueryDataClass
(
  select: Select,
  from: From,
  where: Where,
  override val softType: Type
) extends SQLDataClass

case class DeleteDataClass
(
  table: Table,
  where: Where,
  override val softType: Type,
  findByIdDataClass: FindByIdDataClass,
) extends SQLDataClass

case class InsertDataClass
(
  table: Table,
  entity: SqlEntity,
  findByIdDataClass: FindByIdDataClass,
  override val softType: Type,
  instance: Instance
) extends SQLDataClass


case class UpdateDataClass
(
  table: Table,
  entity: SqlEntity,
  where: Where,
  override val softType: Type,
  findByIdDataClass: FindByIdDataClass,
  instance: Instance
) extends SQLDataClass

case class SqlEntity(values: Seq[SqlValue])
case class SqlValue(column: Column, value: Any)