package dev.rudiments.hardcode.sql.actions

import dev.rudiments.data.Action
import dev.rudiments.data._
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcode.sql.{Binding, SqlEntity, SqlValue}
import dev.rudiments.domain.{Domain, Spec}
import dev.rudiments.hardcore.Reply
import scalikejdbc.{DBSession, SQL}

class UpdateAction(schema: TypedSchema, domain: Domain, spec: Spec)(session: DBSession) extends Action[Update, Updated] {
  override def apply(command: Update): Reply = {
    import command.{key, value}
    implicit val s: DBSession = session
    val table = schema.tables(spec)
    val fieldToColumn = table.columns.map(c => c.name -> c).toMap

    new FindAction(schema, domain, spec)(session)(Find(key)).flatMap[Found] { toUpdate =>
      val where = wherePart(idToWhere(table, spec)(key))

      val entity = SqlEntity(spec.fields.keys.map { field =>
        SqlValue(fieldToColumn(field), value.extract[Any](field))
      }.toSeq)

      val entityBindings = entity.values.map { case SqlValue(column, value) => Binding(column.name, value) }.toSet

      val setPart = entity.values.map(value => s"${value.column.name} = {${value.column.name}}").mkString(", ")

      SQL(
        s"""
           |UPDATE ${schema.name}.${table.name}
           |SET $setPart
           |WHERE ${where.sql}
           |""".stripMargin,
      ).bindByName((where.bindings ++ entityBindings).map(_.toScalike) :_*).execute().apply()(session)

      new FindAction(schema, domain, spec)(session)(Find(key)) match {
        case found: Found => Updated(key, toUpdate.value, found.value)
        case _: NotFound => FailedToUpdate(key, toUpdate.value)
      }
    }
  }
}
