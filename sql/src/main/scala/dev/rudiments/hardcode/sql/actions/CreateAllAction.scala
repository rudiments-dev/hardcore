package dev.rudiments.hardcode.sql.actions

import dev.rudiments.data.Action
import dev.rudiments.data.Batch.{AllCreated, CreateAll}
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcode.sql.{Binding, SqlEntity, SqlValue}
import dev.rudiments.hardcore.Result
import dev.rudiments.hardcore.types.Type
import scalikejdbc.{DBSession, SQL}

class CreateAllAction(schema: TypedSchema, t: Type)(session: DBSession) extends Action[CreateAll, AllCreated] {
  override def apply(command: CreateAll): Result[AllCreated] = {
    command.batch match {
      case batch if batch.isEmpty => AllCreated(batch).toEither
      case batch =>
        implicit val s = session
        val table = schema.tables(t)
        val fieldToColumn = table.columns.map(c => c.name -> c).toMap

        val entities = batch.values.map { entity =>
          SqlEntity(t.fields.keys.map { field =>
            SqlValue(fieldToColumn(field), t.extract(entity, field))
          }.toSeq)
        }
        val bindings: Seq[Seq[(Symbol, Any)]] = entities.map(_.values
          .map { case SqlValue(column, value) => Binding(column.name, value) }
          .map {
            Binding.toScalaLikeSQL
          }
        ).toSeq

        SQL(
          s"""
             |INSERT INTO ${schema.name}.${table.name} (${t.fields.mkString(", ")})
             |VALUES (${t.fields.map(field => s"{$field}").mkString(", ")})
             |""".stripMargin
        ).batchByName(bindings: _*).apply()

        AllCreated(batch).toEither
    }
  }
}