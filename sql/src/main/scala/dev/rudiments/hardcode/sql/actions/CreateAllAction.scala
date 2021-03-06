package dev.rudiments.hardcode.sql.actions

import dev.rudiments.data.Action
import dev.rudiments.data._
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcode.sql.{Binding, SqlEntity, SqlValue}
import dev.rudiments.domain.Spec
import scalikejdbc.{DBSession, SQL}

class CreateAllAction(schema: TypedSchema, spec: Spec)(session: DBSession) extends Action[CreateAll, Commit] {
  override def apply(command: CreateAll): Commit = {
    command.batch match {
      case batch if batch.isEmpty => Commit(Map.empty)
      case batch =>
        implicit val s = session
        val table = schema.tables(spec)
        val fieldToColumn = table.columns.map(c => c.name -> c).toMap

        val entities = batch.values.map { entity =>
          SqlEntity(spec.fields.keys.map { field =>
            SqlValue(fieldToColumn(field), entity.extract[Any](field))
          }.toSeq)
        }
        val bindings: Seq[Seq[(Symbol, Any)]] = entities.map(_.values
          .map { case SqlValue(column, value) => Binding(column.name, value) }
          .map(_.toScalike)
        ).toSeq

        SQL(
          s"""
             |INSERT INTO ${schema.name}.${table.name} (${table.columns.map(_.name).mkString(", ")})
             |VALUES (${table.columns.map(column => s"{${column.name}}").mkString(", ")})
             |""".stripMargin
        ).batchByName(bindings: _*).apply()

        Commit(Map.empty)
    }
  }
}