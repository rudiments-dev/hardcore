package dev.rudiments.hardcode.sql.actions

import dev.rudiments.data.Action
import dev.rudiments.data.Batch.{AllReplaced, CreateAll, DeleteAll, ReplaceAll}
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.domain.Spec
import scalikejdbc.DBSession

class ReplaceAllAction(schema: TypedSchema, spec: Spec)(session: DBSession) extends Action[ReplaceAll, AllReplaced] {
  override def apply(command: ReplaceAll): AllReplaced = {
    new DeleteAllAction(schema, spec)(session)(DeleteAll())
    val created = new CreateAllAction(schema, spec)(session)(CreateAll(command.batch))

    AllReplaced(created.batch)
  }
}