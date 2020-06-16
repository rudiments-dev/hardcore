package dev.rudiments.hardcode.sql.skills

import dev.rudiments.data
import dev.rudiments.data.Batch.{AllDeleted, DeleteAll}
import dev.rudiments.data.DataSkill
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcore.{Command, Result, Success}
import dev.rudiments.hardcore.types.Type
import scalikejdbc.{DBSession, SQL}

class DeleteAllSkill(schema: TypedSchema, t: Type)(session: DBSession) extends DataSkill {

  override def isDefinedAt(x: Command): Boolean = x match {
    case _ : DeleteAll.type => true
    case _ => false
  }

  override def apply(v1: Command): Result[AllDeleted] = v1 match {
    case DeleteAll =>
      val table = schema.tables(t)

      SQL(
        s"""
          |DELETE FROM ${schema.name}.${table.name}
        """.stripMargin
      ).execute().apply()(session)

      AllDeleted().toEither
  }
}
