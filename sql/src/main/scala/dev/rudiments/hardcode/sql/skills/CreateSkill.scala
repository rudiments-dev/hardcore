package dev.rudiments.hardcode.sql.skills

import dev.rudiments.data
import dev.rudiments.data.CRUD.{AlreadyExists, Create, Created, FailedToCreate}
import dev.rudiments.data.DataSkill
import dev.rudiments.data.ReadOnly.{Find, NotFound}
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcode.sql.{Binding, SqlEntity, SqlValue}
import dev.rudiments.hardcore.types.Type
import dev.rudiments.hardcore.{Command, Result}
import scalikejdbc.{DBSession, SQL}


class CreateSkill(schema: TypedSchema, t: Type)(session: DBSession) extends DataSkill {

  override def isDefinedAt(x: Command): Boolean = x match {
    case Create(_, _) => true
    case _ => false
  }

  override def apply(v1: Command): Result[Created] = v1 match {
    case Create(key, value) =>
      val table = schema.tables(t)
      val fieldToColumn = table.columns.map(c => c.name -> c).toMap

      val entity = SqlEntity(t.fields.keys.map { field =>
        SqlValue(fieldToColumn(field), t.extract(value, field))
      }.toSeq)


      val fields: Seq[String] = entity.values.map(_.column.name)

      for {
        _ <- new FindSkill(schema, t)(session)(Find(key)).map {
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
        created <- new FindSkill(schema, t)(session)(Find(key)).transform(
          _ => FailedToCreate(key, value),
          found => Created(found.key, found.value)
        )
      } yield created
  }
}
