package dev.rudiments.hardcode.sql.scalalike

import dev.rudiments.hardcode.sql.materializer.{Binding, SQLMaterializer}
import dev.rudiments.hardcode.sql.SQLDataClasses._


class ScalaLikeSQLMaterializer extends SQLMaterializer[ScalaLikeSQL] {

  def insertSQL(insert: InsertDataClass): CreateSQL = {
    val fields: Seq[String] = insert.entity.values.map(_.column.name)

    val bindings = insert.entity.values.map { case SqlValue(column, value) => Binding(column.name, value) }
    CreateSQL(
      s"""
         |INSERT INTO ${insert.schema.name}.${insert.table.name} (${fields.mkString(", ")})
         |VALUES (${fields.map(field => s"{$field}").mkString(", ")})
         |""".stripMargin,
      bindings.toSet,
      insert.softType,
      findByIdSQL(insert.findByIdDataClass),
      insert.instance
    )
  }

  def findByIdSQL(findById: FindByIdDataClass): FindByIDSQL = {
    val (whereSQL, bindings) = wherePart(findById.where)

    FindByIDSQL(
      s"""
         |SELECT ${selectPart(findById.select)}
         |FROM ${fromPart(findById.from)}
         |WHERE $whereSQL
      |""".stripMargin,
      bindings,
      findById.softType,
      findById.id
    )
  }

  def dropSQL(delete: DeleteDataClass): DropSQL = {
    val (whereSQL, bindings) = wherePart(delete.where)
    DropSQL(
      s"""
         |DELETE FROM ${delete.schema.name}.${delete.table.name}
         |WHERE $whereSQL
         |""".stripMargin,
      bindings,
      delete.softType,
      findByIdSQL(delete.findByIdDataClass)
    )
  }

  def updateSQL(update: UpdateDataClass): UpdateSQL = {
    val (whereSQL, whereBindings) = wherePart(update.where)

    val bindings = update.entity.values.map { case SqlValue(column, value) => Binding(column.name, value) }.toSet
    val setPart = update.entity.values.map(value => s"${value.column.name} = {${value.column.name}}").mkString(", ")
    UpdateSQL(
      s"""
      |UPDATE ${update.schema.name}.${update.table.name}
      |SET $setPart
      |WHERE $whereSQL
      |""".stripMargin,
      (whereBindings ++ bindings),
      update.softType,
      findByIdSQL(update.findByIdDataClass)
    )
  }

}
