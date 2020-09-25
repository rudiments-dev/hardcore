package dev.rudiments.hardcode.sql.actions

import dev.rudiments.data.Action
import dev.rudiments.data.Batch.{AllReplaced, CreateAll, DeleteAll, ReplaceAll}
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcore.Result
import dev.rudiments.domain.Spec
import scalikejdbc.DBSession

class ReplaceAllAction(schema: TypedSchema, spec: Spec)(session: DBSession) extends Action[ReplaceAll, AllReplaced] {
  override def apply(command: ReplaceAll): Result[AllReplaced] = {
    for {
      deleted <- new DeleteAllAction(schema, spec)(session)(DeleteAll())
      updated <- new CreateAllAction(schema, spec)(session)(CreateAll(command.batch)).map(created => AllReplaced(created.batch))
    } yield updated
  }
}