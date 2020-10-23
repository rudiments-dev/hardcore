package dev.rudiments.hardcode.sql

import dev.rudiments.domain.{ID, Spec}
import dev.rudiments.hardcode.sql.SQLParts.{ColumnPredicate, Where}
import dev.rudiments.hardcode.sql.schema.{Column, Table}
import dev.rudiments.hardcore
import dev.rudiments.hardcore.{FieldExpression, ParameterExpression, Predicate}

package object actions {

  def wherePart(where: Where): SqlPart = {
    val expressions = where.expressions.map {
      case exp@ColumnPredicate(column, predicate) =>
        val code = math.abs(exp.hashCode()).toString
        val bindingKey = s"${code}_${column.name}"
        predicate.sql(column.name, bindingKey)
    }
    if(expressions.size > 2)
      expressions.reduce(_ and _)
    else if(expressions.size == 1) {
      expressions.head
    } else {
      SqlPart("")
    }
  }

  def idToWhere(table: Table, spec: Spec): PartialFunction[ID, Where] = {
    case id if id.values.isEmpty => throw new UnsupportedOperationException("ID without values not supported for WHERE expression")
    case id =>
      val keys = table.pk.map(_.name)
      Where(
        id.values.map { it =>
          ColumnPredicate(
            table.columns.map(c => c.name -> c).toMap.apply(keys.head),
            SQLPredicates.Equals(it)
          )
        }.toSet
      )
  }

  def partToWhereExpression(table: Table): PartialFunction[Predicate, ColumnPredicate] = {
    val fieldToColumn = table.columns.map(c => c.name -> c).toMap

    {
      case hardcore.Equals(FieldExpression(field), ParameterExpression(value)) =>
        ColumnPredicate(fieldToColumn(field), SQLPredicates.Equals(value))
      case hardcore.More(FieldExpression(field), ParameterExpression(value)) =>
        ColumnPredicate(fieldToColumn(field), SQLPredicates.Greater(value))
      case hardcore.MoreOrEquals(FieldExpression(field), ParameterExpression(value)) =>
        ColumnPredicate(fieldToColumn(field), SQLPredicates.GreaterOrEquals(value))
      case hardcore.Less(FieldExpression(field), ParameterExpression(value)) =>
        ColumnPredicate(fieldToColumn(field), SQLPredicates.Less(value))
      case hardcore.LessOrEquals(FieldExpression(field), ParameterExpression(value)) =>
        ColumnPredicate(fieldToColumn(field), SQLPredicates.LessOrEquals(value))
    }
  }
}

case class SqlEntity(values: Seq[SqlValue])
case class SqlValue(column: Column, value: Any)