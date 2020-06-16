package dev.rudiments.hardcode.sql.skills

import dev.rudiments.data
import dev.rudiments.data.Batch.{AllCreated, AllDeleted, AllReplaced, CreateAll, DeleteAll, ReplaceAll}
import dev.rudiments.data.{DataEvent, DataSkill}
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcore.{Command, Result}
import dev.rudiments.hardcore.types.Type
import scalikejdbc.DBSession

class ReplaceAllSkill(schema: TypedSchema, t: Type)(session: DBSession) extends DataSkill {

  override def isDefinedAt(x: Command): Boolean = x match {
    case _ : ReplaceAll => true
    case _ => false
  }

  override def apply(v1: Command): Result[AllReplaced] = v1 match {
    case ReplaceAll(batch) =>
      for {
        deleted <- new DeleteAllSkill(schema, t)(session)(DeleteAll)
        updated <- new CreateAllSkill(schema, t)(session)(CreateAll(batch)).map(created => AllReplaced(created.batch))
      } yield updated
  }
}
