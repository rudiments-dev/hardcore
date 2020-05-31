package dev.rudiments.hardcode.sql

import SQLParts._
import dev.rudiments.hardcode.sql.schema.{Column, TypedSchema, Table}
import dev.rudiments.hardcore.types.{ID, Instance, Type}

object SQLDataClasses {

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
    where: Option[Where],
    override val softType: Type
  ) extends SQLDataClass

  case class DeleteDataClass
  (
    schema: TypedSchema,
    table: Table,
    where: Where,
    override val softType: Type,
    findByIdDataClass: FindByIdDataClass,
  ) extends SQLDataClass

  case class DeleteAllDataClass
  (
    schema: TypedSchema,
    table: Table,
    override val softType: Type,
  ) extends SQLDataClass

  case class InsertDataClass
  (
    schema: TypedSchema,
    table: Table,
    entity: SqlEntity,
    findByIdDataClass: FindByIdDataClass,
    override val softType: Type,
    instance: Instance
  ) extends SQLDataClass


  case class UpdateDataClass
  (
    schema: TypedSchema,
    table: Table,
    entity: SqlEntity,
    where: Where,
    override val softType: Type,
    findByIdDataClass: FindByIdDataClass,
    instance: Instance
  ) extends SQLDataClass

  case class SqlEntity(values: Seq[SqlValue])
  case class SqlValue(column: Column, value: Any)
}
