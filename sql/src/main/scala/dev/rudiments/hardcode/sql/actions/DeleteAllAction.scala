package dev.rudiments.hardcode.sql.actions

import dev.rudiments.data.Action
import dev.rudiments.data._
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.domain.Spec
import scalikejdbc.{DBSession, SQL}

class DeleteAllAction(schema: TypedSchema, spec: Spec)(session: DBSession) extends Action[DeleteUsing, Commit] {
  override def apply(command: DeleteUsing): Commit = {
    val table = schema.tables(spec)

    SQL(
      s"""
         |DELETE FROM ${schema.name}.${table.name}
        """.stripMargin
    ).execute().apply()(session)

    Commit(Map.empty)
  }
}