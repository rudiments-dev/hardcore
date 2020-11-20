package dev.rudiments.hardcode.sql.actions

import dev.rudiments.data.Action
import dev.rudiments.data._
import dev.rudiments.hardcode.sql.SQLParts.{From, Select, SelectField, Where}
import dev.rudiments.hardcode.sql.schema.TypedSchema
import dev.rudiments.domain.{Domain, ID, Spec}
import dev.rudiments.hardcore.{All, TypedPredicate}
import scalikejdbc.{DBSession, SQL}

class QueryAction(schema: TypedSchema, domain: Domain, spec: Spec)(session: DBSession) extends Action[FindAll, FoundAll] {
  override def apply(command: FindAll): FoundAll = {

    implicit val s: DBSession = session
    val t = spec
    val table = schema.tables(t)
    val fieldToColumn = table.columns.map(c => c.name -> c).toMap

    val selectors = spec.fields.keys.map { field =>
      SelectField(fieldToColumn(field))
    }.toSeq

    val converterFunction = partToWhereExpression(table)
    val instances = command.predicate match {
      case All =>
        SQL(s"""
               |SELECT ${Select(selectors).sql}
               |FROM ${From(schema, table).sql}
               |""".stripMargin
        ).map { rs =>
          spec.fromMap(domain, rs.toMap())
        }.list().apply()

      case TypedPredicate(spec, parts) =>
        val where = wherePart(Where(parts.map(converterFunction).toSet))
        if(where.sql.trim.nonEmpty) {
          SQL(s"""
                 |SELECT ${Select(selectors).sql}
                 |FROM ${From(schema, table).sql}
                 |WHERE ${where.sql}
                 |""".stripMargin
          ).bindByName(where.bindings.map(_.toScalike): _*).map { rs =>
            spec.fromMap(domain, rs.toMap())
          }.list().apply()
        } else {
          SQL(s"""
                 |SELECT ${Select(selectors).sql}
                 |FROM ${From(schema, table).sql}
                 |""".stripMargin
          ).map { rs =>
            spec.fromMap(domain, rs.toMap())
          }.list().apply()
        }

    }
    FoundAll(
      instances.zipWithIndex.map{
        case (i, id) => ID(Seq(id)) -> i
      }.toMap
    )
  }
}