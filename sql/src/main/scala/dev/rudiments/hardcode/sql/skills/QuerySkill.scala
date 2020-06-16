package dev.rudiments.hardcode.sql.skills

import dev.rudiments.data.DataSkill
import dev.rudiments.data.ReadOnly.{FindAll, FoundAll}
import dev.rudiments.hardcode.sql.Binding
import dev.rudiments.hardcode.sql.SQLParts.{From, Select, SelectField, Where}
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcore.{Command, Event, Result, SkillResult, Success}
import dev.rudiments.hardcore.http.query.{PassAllQuery, PredicatesQuery}
import dev.rudiments.hardcore.types.Type
import scalikejdbc.{DBSession, SQL}

class QuerySkill(schema: TypedSchema, t: Type)(session: DBSession) extends DataSkill {

  override def isDefinedAt(x: Command): Boolean = x match {
    case FindAll(_) => true
    case _ => false
  }

  override def apply(v1: Command): Result[FoundAll] = v1 match {
    case FindAll(query) =>
      implicit val s = session
      val t = query.softType
      val table = schema.tables(t)
      val fieldToColumn = table.columns.map(c => c.name -> c).toMap

      val selectors = t.fields.keys.map { field =>
        SelectField(
          fieldToColumn(field), None
        )
      }.toSeq

      val converterFunction = partToWhereExpression(table)
      val instances = query match {
        case PassAllQuery(softType) =>

          SQL(
            s"""
               |SELECT ${selectPart(Select(selectors))}
               |FROM ${fromPart(From(schema, table, None))}
               |""".stripMargin
          ).map { rs =>
            softType.construct(rs.toMap().values.toSeq: _*) //todo refactor to Map
          }.list().apply()

        case PredicatesQuery(parts, softType) =>
          val (whereSQL, whereBindings) = wherePart(Where(parts.map(converterFunction)))

          SQL(
            s"""
               |SELECT ${selectPart(Select(selectors))}
               |FROM ${fromPart(From(schema, table, None))}
               |WHERE $whereSQL
               |""".stripMargin
          ).bindByName(whereBindings.map(Binding.toScalaLikeSQL): _*).map { rs =>
            softType.construct(rs.toMap().values.toSeq: _*) //todo refactor to Map
          }.list().apply()
      }
      Success(FoundAll(instances))
  }
}