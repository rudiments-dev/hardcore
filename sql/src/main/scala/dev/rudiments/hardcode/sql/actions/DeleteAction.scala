package dev.rudiments.hardcode.sql.actions

import dev.rudiments.data
import dev.rudiments.data.CRUD.{Delete, Deleted, FailedToDelete}
import dev.rudiments.data.{Action, DataSkill}
import dev.rudiments.data.ReadOnly.{Find, Found, NotFound}
import dev.rudiments.hardcode.sql.Binding
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcore.{Command, Result}
import dev.rudiments.hardcore.types.Type
import scalikejdbc.{DBSession, SQL}

class DeleteAction(schema: TypedSchema, t: Type)(session: DBSession) extends Action[Delete, Deleted] {
  override def apply(command: Delete): Result[Deleted] = {
    import command.key
    val table = schema.tables(t)

    for {
      found <- new FindAction(schema, t)(session)(Find(key))
      _ = {
        val (whereSQL, bindings) = wherePart(idToWhere(table, t)(key))

        SQL(
          s"""
             |DELETE FROM ${schema.name}.${table.name}
             |WHERE $whereSQL
             |""".stripMargin,
        ).bindByName(bindings.map(Binding.toScalaLikeSQL) :_*).execute().apply()(session)
      }
      _ <- new FindAction(schema, t)(session)(Find(key))
        .expecting[NotFound]
        .recover(_ => FailedToDelete(found.key, found.value))
    } yield Deleted(key, found.value)
  }
}
