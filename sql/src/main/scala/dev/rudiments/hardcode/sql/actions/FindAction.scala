package dev.rudiments.hardcode.sql.actions

import dev.rudiments.data._
import dev.rudiments.domain.{Domain, Spec}
import dev.rudiments.hardcode.sql.Binding
import dev.rudiments.hardcode.sql.SQLParts.{From, Select, SelectField}
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcore.Reply
import scalikejdbc.{DBSession, SQL}

class FindAction(schema: TypedSchema, domain: Domain, spec: Spec)(session: DBSession) extends Action[Find, Found] {
  override def apply(command: Find): Reply = {
    import command.key
    implicit val s = session
    val table = schema.tables(spec)
    val fieldToColumn = table.columns.map(c => c.name -> c).toMap

    val select = Select(spec.fields.keys.map { field =>
      SelectField(fieldToColumn(field))
    }.toSeq)

    val where = wherePart(idToWhere(table, spec)(key))

    SQL(
      s"""
         |SELECT ${select.sql}
         |FROM ${From(schema, table).sql}
         |WHERE ${where.sql}
         |""".stripMargin,
    ).bindByName(where.bindings.map(_.toScalike) : _*).map { rs =>
      spec.fromMap(domain, rs.toMap())
    }.single().apply() match {
      case Some(value) => Found(key, value)
      case None => NotFound(key)
    }
  }
}