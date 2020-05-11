package dev.rudiments.hardcore.sql.materializer.realization.scalalike

import dev.rudiments.hardcore.sql.materializer.{Binding, SQLMaterializer}
import dev.rudiments.hardcore.sql._

class ScalaLikeSQLMaterializer extends SQLMaterializer[ScalaLikeSQL] {

  override def materialize: PartialFunction[SQLDataClass, ScalaLikeSQL] = {
    case SelectSql(select, from, where) =>
      val (whereSQL, bindings) = wherePart(where)
      ScalaLikeSQL(
        s"""
           |SELECT ${selectPart(select)}
           |FROM ${fromPart(from)}
           |WHERE $whereSQL
           |""".stripMargin,
        bindings
      )
    case UpdateSql(table, entity, where) =>
      val (whereSQL, whereBindings) = wherePart(where)

      val bindings = entity.values.map { case SqlValue(column, value) => Binding(column.name, value) }.toSet
      val setPart = entity.values.map(value => s"${value.column} = {${value.column}}").mkString(", ")
      ScalaLikeSQL(
        s"""
           |UPDATE ${table.name}
           |SET $setPart
           |WHERE $whereSQL
           |""".stripMargin,
        (whereBindings ++ bindings)
      )
    case DeleteSql(table, where) =>
      val (whereSQL, bindings) = wherePart(where)
      ScalaLikeSQL(
        s"""
           |DELETE FROM ${table.name}}
           |WHERE $whereSQL
           |""".stripMargin,
        bindings
      )
    case InsertSql(table, entity) =>
      val fields: Seq[String] = entity.values.map(_.column.name)

      val bindings = entity.values.map { case SqlValue(column, value) => Binding(column.name, value) }
      ScalaLikeSQL(
        s"""
           |INSERT INTO ${table.name} (${fields.mkString(", ")}) }
           |VALUES (${fields.map(field => s"{$field}").mkString(", ")})
           |""".stripMargin,
        bindings.toSet
      )
  }

}
