package dev.rudiments.hardcode.sql.actions

import dev.rudiments.data.Action
import dev.rudiments.data._
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.domain.Spec
import dev.rudiments.hardcore.All
import scalikejdbc.DBSession

class ReplaceAllAction(schema: TypedSchema, spec: Spec)(session: DBSession) extends Action[ReplaceAll, Commit] {
  override def apply(command: ReplaceAll): Commit = {
    val deleted = new DeleteAllAction(schema, spec)(session)(DeleteUsing(All))
    val created = new CreateAllAction(schema, spec)(session)(CreateAll(command.batch))

    Commit(deleted.state ++ created.state)
  }
}