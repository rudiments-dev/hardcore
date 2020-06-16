package dev.rudiments.hardcode.sql.skills

import dev.rudiments.data.DataSkill
import dev.rudiments.data.ReadOnly.{Find, Found, NotFound}
import dev.rudiments.hardcode.sql.Binding
import dev.rudiments.hardcode.sql.SQLParts.{From, Select, SelectField}
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcore.types.{SoftID, Type}
import dev.rudiments.hardcore.{Command, Result}
import scalikejdbc.{DBSession, SQL}

class FindSkill(schema: TypedSchema, t: Type)(session: DBSession) extends DataSkill {

  override def isDefinedAt(x: Command): Boolean = x match {
    case Find(_) => true
    case _ => false
  }

  override def apply(v1: Command): Result[Found] = v1 match {
    case Find(key) =>
      implicit val s = session
      val t = key.asInstanceOf[SoftID].t
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
        t.construct(rs.toMap().values.toSeq :_*) //todo refactor to Map
      }.single().apply() match {
        case Some(value) => Found(key, value).toEither
        case None => NotFound(key).toEither
      }
  }
}