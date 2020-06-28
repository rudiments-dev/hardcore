package dev.rudiments.hardcode.sql.skills

import dev.rudiments.{data, hardcore}
import dev.rudiments.data.CRUD.{FailedToUpdate, Update, Updated}
import dev.rudiments.data.DataSkill
import dev.rudiments.data.ReadOnly.{Find, Found, NotFound}
import dev.rudiments.hardcode.sql.{Binding, SqlEntity, SqlValue}
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcore.{Command, Event, Message, Result, SkillResult}
import dev.rudiments.hardcore.types.Type
import scalikejdbc.{DBSession, SQL}

class UpdateSkill(schema: TypedSchema, t: Type)(session: DBSession) extends DataSkill {

  override def isDefinedAt(x: Command): Boolean = x match {
    case Update(_, _) => true
    case _ => false
  }

  override def apply(v1: Command): Result[Updated] = v1 match {
    case Update(key, value) =>
      implicit val s: DBSession = session
      val table = schema.tables(t)
      val fieldToColumn = table.columns.map(c => c.name -> c).toMap

      for {
        toUpdate <- new FindSkill(schema, t)(session)(Find(key))
        _ = {
          val (whereSQL, whereBindings) = wherePart(idToWhere(table, t)(key))

          val entity = SqlEntity(t.fields.keys.map { field =>
            SqlValue(fieldToColumn(field), t.extract(value, field))
          }.toSeq)

          val entityBindings = entity.values.map { case SqlValue(column, value) => Binding(column.name, value) }.toSet

          val setPart = entity.values.map(value => s"${value.column.name} = {${value.column.name}}").mkString(", ")

          SQL(
            s"""
               |UPDATE ${schema.name}.${table.name}
               |SET $setPart
               |WHERE $whereSQL
               |""".stripMargin,
          ).bindByName((whereBindings ++ entityBindings).map(Binding.toScalaLikeSQL) :_*).execute().apply()(session)
        }
        updated <- new FindSkill(schema, t)(session)(Find(key)).transform(
          f = _ => FailedToUpdate(key, toUpdate.value),
          g = found => Updated(key, toUpdate.value, found.value)
        )
      } yield updated
  }
}
