package dev.rudiments.hardcode.sql.actions

import dev.rudiments.data.{Action, DataSkill}
import dev.rudiments.data.ReadOnly.{FindAll, FoundAll}
import dev.rudiments.hardcode.sql.Binding
import dev.rudiments.hardcode.sql.SQLParts.{From, Select, SelectField, Where}
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcore.{Command, Event, Result, SkillResult, Success}
import dev.rudiments.hardcore.http.query.{PassAllQuery, PredicatesQuery}
import dev.rudiments.hardcore.types.Type
import scalikejdbc.{DBSession, SQL}

class QueryAction(schema: TypedSchema, t: Type)(session: DBSession) extends Action[FindAll, FoundAll] {
  override def apply(command: FindAll): Result[FoundAll] = {
    import command.query

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
          softType.constructFromMap(rs.toMap())
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
          softType.constructFromMap(rs.toMap())
        }.list().apply()
    }
    Success(FoundAll(instances))
  }
}