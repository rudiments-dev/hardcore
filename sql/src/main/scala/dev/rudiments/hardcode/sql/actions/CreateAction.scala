package dev.rudiments.hardcode.sql.actions

import dev.rudiments.data.Action
import dev.rudiments.data.CRUD.{AlreadyExists, Create, Created, FailedToCreate}
import dev.rudiments.data.ReadOnly.{Find, NotFound}
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcode.sql.{Binding, SqlEntity, SqlValue}
import dev.rudiments.hardcore.Result
import dev.rudiments.types.Type
import scalikejdbc.{DBSession, SQL}


class CreateAction(schema: TypedSchema, t: Type)(session: DBSession) extends Action[Create, Created] {
  override def apply(command: Create): Result[Created] = {
    import command.{key, value}
    val table = schema.tables(t)
    val fieldToColumn = table.columns.map(c => c.name -> c).toMap

    val entity = SqlEntity(t.fields.keys.map { field =>
      SqlValue(fieldToColumn(field), value.extract[Any](field))
    }.toSeq)


    val fields: Seq[String] = entity.values.map(_.column.name)

    for {
      _ <- new FindAction(schema, t)(session)(Find(key)).map {
        found => AlreadyExists(found.key, found.value)
      }.expecting[NotFound]
      _ = {
        val bindings = entity.values.map { case SqlValue(column, value) => Binding(column.name, value) }.map(Binding.toScalaLikeSQL)

        SQL(
          s"""
             |INSERT INTO ${schema.name}.${table.name} (${fields.mkString(", ")})
             |VALUES (${fields.map(field => s"{$field}").mkString(", ")})
             |""".stripMargin
        ).bindByName(bindings :_*).execute().apply()(session)
      }
      created <- new FindAction(schema, t)(session)(Find(key)).transform(
        _ => FailedToCreate(key, value),
        found => Created(found.key, found.value)
      )
    } yield created
  }
}
