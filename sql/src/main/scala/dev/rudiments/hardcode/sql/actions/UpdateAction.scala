package dev.rudiments.hardcode.sql.actions

import dev.rudiments.data.Action
import dev.rudiments.data.CRUD.{FailedToUpdate, Update, Updated}
import dev.rudiments.data.ReadOnly.Find
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcode.sql.{Binding, SqlEntity, SqlValue}
import dev.rudiments.hardcore.Result
import dev.rudiments.domain.{Domain, Spec}
import scalikejdbc.{DBSession, SQL}

import cats.syntax.either._

class UpdateAction(schema: TypedSchema, domain: Domain, spec: Spec)(session: DBSession) extends Action[Update, Updated] {
  override def apply(command: Update): Result[Updated] = {
    import command.{key, value}
    implicit val s: DBSession = session
    val table = schema.tables(spec)
    val fieldToColumn = table.columns.map(c => c.name -> c).toMap

    for {
      toUpdate <- new FindAction(schema, domain, spec)(session)(Find(key))
      _ = {
        val (whereSQL, whereBindings) = wherePart(idToWhere(table, spec)(key))

        val entity = SqlEntity(spec.fields.keys.map { field =>
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
      updated <- new FindAction(schema, domain, spec)(session)(Find(key)).bimap(
        _ => FailedToUpdate(key, toUpdate.value),
        found => Updated(key, toUpdate.value, found.value)
      )
    } yield updated
  }
}
