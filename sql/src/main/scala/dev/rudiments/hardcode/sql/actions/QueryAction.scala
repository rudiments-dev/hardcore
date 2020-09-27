package dev.rudiments.hardcode.sql.actions

import dev.rudiments.data.Action
import dev.rudiments.data.ReadOnly.{FindAll, FoundAll}
import dev.rudiments.hardcode.sql.Binding
import dev.rudiments.hardcode.sql.SQLParts.{From, Select, SelectField, Where}
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.hardcore.http.query.{PassAllQuery, PredicatesQuery}
import dev.rudiments.domain.{Domain, Spec}
import scalikejdbc.{DBSession, SQL}

class QueryAction(schema: TypedSchema, domain: Domain, spec: Spec)(session: DBSession) extends Action[FindAll, FoundAll] {
  override def apply(command: FindAll): FoundAll = {
    import command.query

    implicit val s: DBSession = session
    val t = query.spec
    val table = schema.tables(t)
    val fieldToColumn = table.columns.map(c => c.name -> c).toMap

    val selectors = spec.fields.keys.map { field =>
      SelectField(
        fieldToColumn(field), None
      )
    }.toSeq

    val converterFunction = partToWhereExpression(table)
    val instances = query match {
      case PassAllQuery(sp) =>

        SQL(
          s"""
             |SELECT ${selectPart(Select(selectors))}
             |FROM ${fromPart(From(schema, table, None))}
             |""".stripMargin
        ).map { rs =>
          sp.fromMap(domain, rs.toMap())
        }.list().apply()

      case PredicatesQuery(parts, sp) =>
        val (whereSQL, whereBindings) = wherePart(Where(parts.map(converterFunction)))

        SQL(
          s"""
             |SELECT ${selectPart(Select(selectors))}
             |FROM ${fromPart(From(schema, table, None))}
             |WHERE $whereSQL
             |""".stripMargin
        ).bindByName(whereBindings.map(Binding.toScalaLikeSQL): _*).map { rs =>
          sp.fromMap(domain, rs.toMap())
        }.list().apply()
    }
    FoundAll(instances)
  }
}