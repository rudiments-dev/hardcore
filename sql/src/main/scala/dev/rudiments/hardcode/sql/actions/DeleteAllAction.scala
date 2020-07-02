package dev.rudiments.hardcode.sql.actions

import dev.rudiments.data
import dev.rudiments.data.Batch.{AllDeleted, DeleteAll}
import dev.rudiments.data.{Action, DataSkill}
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcore.{Command, Result, Success}
import dev.rudiments.hardcore.types.Type
import scalikejdbc.{DBSession, SQL}

class DeleteAllAction(schema: TypedSchema, t: Type)(session: DBSession) extends Action[DeleteAll, AllDeleted] {
  override def apply(command: DeleteAll): Result[AllDeleted] = {
    val table = schema.tables(t)

    SQL(
      s"""
         |DELETE FROM ${schema.name}.${table.name}
        """.stripMargin
    ).execute().apply()(session)

    AllDeleted().toEither
  }
}