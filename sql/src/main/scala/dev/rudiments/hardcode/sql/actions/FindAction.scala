package dev.rudiments.hardcode.sql.actions

import dev.rudiments.data.Action
import dev.rudiments.data.ReadOnly.{Find, Found, NotFound}
import dev.rudiments.hardcode.sql.Binding
import dev.rudiments.hardcode.sql.SQLParts.{From, Select, SelectField}
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcore.Result
import dev.rudiments.types.{ID, Type}
import scalikejdbc.{DBSession, SQL}

class FindAction(schema: TypedSchema, t: Type)(session: DBSession) extends Action[Find, Found] {
  override def apply(command: Find): Result[Found] = {
    import command.key
    implicit val s = session
    val t = key.t
    val table = schema.tables(t)
    val fieldToColumn = table.columns.map(c => c.name -> c).toMap

    val select = Select(t.fields.keys.map { field =>
      SelectField(
        fieldToColumn(field), None
      )
    }.toSeq)

    val from = From(schema, table, None)

    val (whereSQL, bindings) = wherePart(idToWhere(table, t)(key))

    SQL(
      s"""
         |SELECT ${selectPart(select)}
         |FROM ${fromPart(from)}
         |WHERE $whereSQL
         |""".stripMargin,
    ).bindByName(bindings.map(Binding.toScalaLikeSQL) : _*).map { rs =>
      t.construct(rs.toMap().values.toSeq :_*)
    }.single().apply() match {
      case Some(value) => Found(key, value).toEither
      case None => NotFound(key).toEither
    }
  }
}