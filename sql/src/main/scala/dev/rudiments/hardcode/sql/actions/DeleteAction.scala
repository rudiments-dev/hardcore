package dev.rudiments.hardcode.sql.actions

import dev.rudiments.data.Action
import dev.rudiments.data.CRUD.{Delete, Deleted, FailedToDelete}
import dev.rudiments.data.ReadOnly.{Find, NotFound}
import dev.rudiments.hardcode.sql.Binding
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcore.Result
import dev.rudiments.domain.{Domain, Spec}
import scalikejdbc.{DBSession, SQL}

import cats.syntax.either._

class DeleteAction(schema: TypedSchema, domain: Domain, spec: Spec)(session: DBSession) extends Action[Delete, Deleted] {
  override def apply(command: Delete): Result[Deleted] = {
    import command.key
    val table = schema.tables(spec)

    for {
      found <- new FindAction(schema, domain, spec)(session)(Find(key))
      _ = {
        val (whereSQL, bindings) = wherePart(idToWhere(table, spec)(key))

        SQL(
          s"""
             |DELETE FROM ${schema.name}.${table.name}
             |WHERE $whereSQL
             |""".stripMargin,
        ).bindByName(bindings.map(Binding.toScalaLikeSQL) :_*).execute().apply()(session)
      }
      _ <- new FindAction(schema, domain, spec)(session)(Find(key))
          .recover { case _ => FailedToDelete(found.key, found.value) }
    } yield Deleted(key, found.value)
  }
}
