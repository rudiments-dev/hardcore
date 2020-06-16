package dev.rudiments.hardcode.sql.skills

import dev.rudiments.data
import dev.rudiments.data.CRUD.{Delete, Deleted, FailedToDelete}
import dev.rudiments.data.DataSkill
import dev.rudiments.data.ReadOnly.{Find, Found, NotFound}
import dev.rudiments.hardcode.sql.Binding
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcore.{Command, Result}
import dev.rudiments.hardcore.types.Type
import scalikejdbc.{DBSession, SQL}

class DeleteSkill(schema: TypedSchema, t: Type)(session: DBSession) extends DataSkill {

  override def isDefinedAt(x: Command): Boolean = x match {
    case Delete(_) => true
    case _ => false
  }

  override def apply(v1: Command): Result[Deleted] =  v1 match {
    case Delete(key) =>
      val table = schema.tables(t)

      for {
        found <- new FindSkill(schema, t)(session)(Find(key))
        _ = {
          val (whereSQL, bindings) = wherePart(idToWhere(table, t)(key))

          SQL(
            s"""
               |DELETE FROM ${schema.name}.${table.name}
               |WHERE $whereSQL
               |""".stripMargin,
          ).bindByName(bindings.map(Binding.toScalaLikeSQL) :_*).execute().apply()(session)
        }
        _ <- new FindSkill(schema, t)(session)(Find(key))
          .expecting[NotFound]
          .recover(_ => FailedToDelete(found.key, found.value))
      } yield Deleted(key, found.value)


  }
}
