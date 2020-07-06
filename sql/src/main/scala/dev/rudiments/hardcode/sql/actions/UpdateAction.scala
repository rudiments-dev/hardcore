package dev.rudiments.hardcode.sql.actions

import dev.rudiments.data.Action
import dev.rudiments.data.CRUD.{FailedToUpdate, Update, Updated}
import dev.rudiments.data.ReadOnly.Find
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcode.sql.{Binding, SqlEntity, SqlValue}
import dev.rudiments.hardcore.Result
import dev.rudiments.types.Type
import scalikejdbc.{DBSession, SQL}

class UpdateAction(schema: TypedSchema, t: Type)(session: DBSession) extends Action[Update, Updated] {
  override def apply(command: Update): Result[Updated] = {
    import command.{key, value}
    implicit val s: DBSession = session
    val table = schema.tables(t)
    val fieldToColumn = table.columns.map(c => c.name -> c).toMap

    for {
      toUpdate <- new FindAction(schema, t)(session)(Find(key))
      _ = {
        val (whereSQL, whereBindings) = wherePart(idToWhere(table, t)(key))

        val entity = SqlEntity(t.fields.keys.map { field =>
          SqlValue(fieldToColumn(field), value.extract[Any](field))
        }.toSeq)

        val entityBindings = entity.values.map { case SqlValue(column, value) => Binding(column.name, value) }.toSet

        val setPart = entity.values.map(value => s"${value.column.name} = {${value.column.name}}").mkString(", ")

        SQL(
          s"""
             |UPDATE ${schema.name}.${table.name}
             |SET $setPart
             |WHERE $whereSQL
             |""".stripMargin,
        ).bindByName((whereBindings ++ entityBindings).map(Binding.toScalaLikeSQL) :_*).execute().apply()(session)
      }
      updated <- new FindAction(schema, t)(session)(Find(key)).transform(
        f = _ => FailedToUpdate(key, toUpdate.value),
        g = found => Updated(key, toUpdate.value, found.value)
      )
    } yield updated
  }
}
