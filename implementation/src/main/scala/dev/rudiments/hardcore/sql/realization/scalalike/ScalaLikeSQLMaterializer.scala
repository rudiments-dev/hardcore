package dev.rudiments.hardcore.sql.realization.scalalike

import dev.rudiments.hardcore.sql.materializer.{Binding, SQLMaterializer}
import dev.rudiments.hardcore.sql._


class ScalaLikeSQLMaterializer extends SQLMaterializer[ScalaLikeSQL] {

  def insertSQL(insert: InsertDataClass): CreateSQL = {
    val fields: Seq[String] = insert.entity.values.map(_.column.name)

    val bindings = insert.entity.values.map { case SqlValue(column, value) => Binding(column.name, value) }
    CreateSQL(
      s"""
         |INSERT INTO ${insert.table.name} (${fields.mkString(", ")}) }
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
         |DELETE FROM ${delete.table.name}}
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
    val setPart = update.entity.values.map(value => s"${value.column} = {${value.column}}").mkString(", ")
    UpdateSQL(
      s"""
      |UPDATE ${update.table.name}
      |SET $setPart
      |WHERE $whereSQL
      |""".stripMargin,
      (whereBindings ++ bindings),
      update.softType,
      findByIdSQL(update.findByIdDataClass)
    )
  }

  //  override def materialize: PartialFunction[SQLDataClass, ScalaLikeSQL] = {
//    case SelectSql(select, from, where) =>
//      val (whereSQL, bindings) = wherePart(where)
//      ScalaLikeSQL(
//        s"""
//           |SELECT ${selectPart(select)}
//           |FROM ${fromPart(from)}
//           |WHERE $whereSQL
//           |""".stripMargin,
//        bindings
//      )
//    case UpdateSql(table, entity, where) =>
//      val (whereSQL, whereBindings) = wherePart(where)
//
//      val bindings = entity.values.map { case SqlValue(column, value) => Binding(column.name, value) }.toSet
//      val setPart = entity.values.map(value => s"${value.column} = {${value.column}}").mkString(", ")
//      ScalaLikeSQL(
//        s"""
//           |UPDATE ${table.name}
//           |SET $setPart
//           |WHERE $whereSQL
//           |""".stripMargin,
//        (whereBindings ++ bindings)
//      )
//    case DeleteSql(table, where) =>
//      val (whereSQL, bindings) = wherePart(where)
//      ScalaLikeSQL(
//        s"""
//           |DELETE FROM ${table.name}}
//           |WHERE $whereSQL
//           |""".stripMargin,
//        bindings
//      )
//    case InsertSql(table, entity) =>
//      val fields: Seq[String] = entity.values.map(_.column.name)
//
//      val bindings = entity.values.map { case SqlValue(column, value) => Binding(column.name, value) }
//      ScalaLikeSQL(
//        s"""
//           |INSERT INTO ${table.name} (${fields.mkString(", ")}) }
//           |VALUES (${fields.map(field => s"{$field}").mkString(", ")})
//           |""".stripMargin,
//        bindings.toSet
//      )

}
