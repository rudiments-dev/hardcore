package dev.rudiments.hardcode.sql.actions

import dev.rudiments.data.Action
import dev.rudiments.data.Batch.{AllDeleted, DeleteAll}
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcore.Result
import dev.rudiments.domain.Spec
import scalikejdbc.{DBSession, SQL}

class DeleteAllAction(schema: TypedSchema, spec: Spec)(session: DBSession) extends Action[DeleteAll, AllDeleted] {
  override def apply(command: DeleteAll): Result[AllDeleted] = {
    val table = schema.tables(spec)

    SQL(
      s"""
         |DELETE FROM ${schema.name}.${table.name}
        """.stripMargin
    ).execute().apply()(session)

    AllDeleted().toEither
  }
}