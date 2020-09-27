package dev.rudiments.hardcode.sql.actions

import dev.rudiments.data.Action
import dev.rudiments.data.CRUD.{AlreadyExists, Create, Created, FailedToCreate}
import dev.rudiments.data.ReadOnly.{Counted, Find, Found, NotFound}
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcode.sql.{Binding, SqlEntity, SqlValue}
import dev.rudiments.domain.{Domain, Spec}
import dev.rudiments.hardcore.Message
import scalikejdbc.{DBSession, SQL}

class CreateAction(schema: TypedSchema, domain: Domain, spec: Spec)(session: DBSession) extends Action[Create, Created] {
  override def apply(command: Create): Message = {
    import command.{key, value}
    val table = schema.tables(spec)
    val fieldToColumn = table.columns.map(c => c.name -> c).toMap

    val entity = SqlEntity(spec.fields.keys.map { field =>
      SqlValue(fieldToColumn(field), value.extract[Any](field))
    }.toSeq)


    val fields: Seq[String] = entity.values.map(_.column.name)

    new FindAction(schema, domain, spec)(session)(Find(key)) match {
      case found: Found => AlreadyExists(found.key, found.value)
      case _: NotFound =>
        val bindings = entity.values.map { case SqlValue(column, value) => Binding(column.name, value) }.map(Binding.toScalaLikeSQL)

        SQL(
          s"""
             |INSERT INTO ${schema.name}.${table.name} (${fields.mkString(", ")})
             |VALUES (${fields.map(field => s"{$field}").mkString(", ")})
             |""".stripMargin
        ).bindByName(bindings :_*).execute().apply()(session)

        new FindAction(schema, domain, spec)(session)(Find(key)) match {
          case found: Found => Created(found.key, found.value)
          case _ => FailedToCreate(key, value)
        }
    }
  }
}
