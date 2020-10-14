package dev.rudiments.hardcode.sql.actions

import dev.rudiments.data.Action
import dev.rudiments.data.CRUD.{Delete, Deleted, FailedToDelete}
import dev.rudiments.data.ReadOnly.{Find, Found, NotFound}
import dev.rudiments.hardcode.sql.Binding
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.domain.{Domain, Spec}
import dev.rudiments.hardcore.Message
import scalikejdbc.{DBSession, SQL}

class DeleteAction(schema: TypedSchema, domain: Domain, spec: Spec)(session: DBSession) extends Action[Delete, Deleted] {
  override def apply(command: Delete): Message = {
    import command.key
    val table = schema.tables(spec)

    new FindAction(schema, domain, spec)(session)(Find(key)).on[Found] { found =>
        val (whereSQL, bindings) = wherePart(idToWhere(table, spec)(key))

        SQL(
          s"""
             |DELETE FROM ${schema.name}.${table.name}
             |WHERE $whereSQL
             |""".stripMargin,
        ).bindByName(bindings.map(Binding.toScalaLikeSQL) :_*).execute().apply()(session)

      new FindAction(schema, domain, spec)(session)(Find(key)) match {
        case f: Found => FailedToDelete(found.key, found.value)
        case _: NotFound => Deleted(key, found.value)
      }
    }
  }
}
