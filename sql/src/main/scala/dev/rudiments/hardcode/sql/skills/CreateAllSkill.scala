package dev.rudiments.hardcode.sql.skills

import dev.rudiments.data
import dev.rudiments.data.Batch.{AllCreated, CreateAll}
import dev.rudiments.data.DataSkill
import dev.rudiments.hardcode.sql.{Binding, SqlEntity, SqlValue}
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcore.{Command, Result}
import dev.rudiments.hardcore.types.Type
import scalikejdbc.{DBSession, SQL}

class CreateAllSkill(schema: TypedSchema, t: Type)(session: DBSession) extends DataSkill {

  override def isDefinedAt(x: Command): Boolean = x match {
    case CreateAll(_) => true
    case _ => false
  }

  override def apply(command: Command): Result[AllCreated] = command match {
    case CreateAll(batch) if batch.nonEmpty =>
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
        .map {Binding.toScalaLikeSQL}
      ).toSeq

      SQL(
        s"""
           |INSERT INTO ${schema.name}.${table.name} (${t.fields.mkString(", ")})
           |VALUES (${t.fields.map(field => s"{$field}").mkString(", ")})
           |""".stripMargin
      ).batchByName(bindings:_*).apply()

      AllCreated(batch).toEither
    case CreateAll(batch) => AllCreated(batch).toEither
  }
}
