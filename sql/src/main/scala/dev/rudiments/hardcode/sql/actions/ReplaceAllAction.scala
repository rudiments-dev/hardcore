package dev.rudiments.hardcode.sql.actions

import dev.rudiments.data.Action
import dev.rudiments.data.Batch.{AllReplaced, CreateAll, DeleteAll, ReplaceAll}
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcore.Result
import dev.rudiments.hardcore.types.Type
import scalikejdbc.DBSession

class ReplaceAllAction(schema: TypedSchema, t: Type)(session: DBSession) extends Action[ReplaceAll, AllReplaced] {
  override def apply(command: ReplaceAll): Result[AllReplaced] = {
    for {
      deleted <- new DeleteAllAction(schema, t)(session)(DeleteAll())
      updated <- new CreateAllAction(schema, t)(session)(CreateAll(command.batch)).map(created => AllReplaced(created.batch))
    } yield updated
  }
}