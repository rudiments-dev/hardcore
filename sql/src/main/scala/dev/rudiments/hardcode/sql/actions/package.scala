package dev.rudiments.hardcode.sql

import dev.rudiments.hardcode.sql.SQLParts.{ColumnWhereExpression, From, Select, SelectField, Where, WhereExpression}
import dev.rudiments.hardcode.sql.SQLPredicates.{Between, Contains, EndsWith, Equals, Greater, GreaterOrEquals, In, IsNull, Less, LessOrEquals, NotEquals, NotNull, StartsWith}
import dev.rudiments.hardcode.sql.schema.{Column, Table}
import dev.rudiments.hardcore.http.query.predicates.{DoubleEquals, DoubleLess, DoubleLessOrEquals, DoubleMore, DoubleMoreOrEquals, FieldPredicate, IntEquals, IntLess, IntLessOrEquals, IntMore, IntMoreOrEquals, IsDefined, IsEmpty, IsFalse, IsTrue, OptionValuePredicate, Predicate, StringContains, StringEndsWith, StringEquals, StringStartsWith}
import dev.rudiments.domain.{ID, Spec}

package object actions {

  def selectPart(select: Select): String = select.selects.map {
    case SelectField(column, Some(as)) => s"${column.name} as $as"
    case SelectField(column, None) => column.name
  }.mkString(", ")

  def fromPart(from: From): String = from match {
    case From(schema, table, Some(as)) => s"${schema.name}.${table.name} as $as"
    case From(schema, table, None) => s"${schema.name}.${table.name}"
  }

  def wherePart(where: Where): (String, Seq[Binding]) = {
    val expressions = where.expressions.map {
      case exp@ColumnWhereExpression(column, predicate) =>
        val code = math.abs(exp.hashCode()).toString
        val bindingKey = s"${code}_${column.name}"
        predicate match {
          case IsNull =>

            (
              s"${column.name} IS NULL",
              Seq.empty
            )
          case NotNull =>
            (
              s"${column.name} IS NOT NULL",
              Seq.empty
            )
          case Equals(value) =>
            (
              s"${column.name} = {$bindingKey}",
              Seq(Binding(bindingKey, value))
            )
          case NotEquals(value) =>
            (
              s"${column.name} != {$bindingKey}",
              Seq(Binding(bindingKey, value))
            )
          case In(in) =>
            val bindings = in.zipWithIndex.map { case (value, i) => Binding(s"${bindingKey}_$i", value)}
            (
              s"${column.name} IN {${bindings.map(_.key).mkString(", ")}}",
              bindings
            )
          case Greater(value) =>
            (
              s"${column.name} > {$bindingKey}",
              Seq(Binding(bindingKey, value))
            )
          case GreaterOrEquals(value) =>
            (
              s"${column.name} >= {$bindingKey}",
              Seq(Binding(bindingKey, value))
            )
          case Less(value) =>
            (
              s"${column.name} < {$bindingKey}",
              Seq(Binding(bindingKey, value))
            )
          case LessOrEquals(value) =>
            (
              s"${column.name} <= {$bindingKey}",
              Seq(Binding(bindingKey, value))
            )
          case Between(from, to) =>
            val left = s"${bindingKey}_left"
            val right = s"${bindingKey}_right"

            (
              s"${column.name} BETWEEN {$left} AND {$right}",
              Seq(Binding(left, from), Binding(right, to))
            )
          case Contains(value) =>
            (
              s"${column.name} LIKE '%{$bindingKey}%'",
              Seq(Binding(bindingKey, value))
            )
          case EndsWith(value) =>
            (
              s"${column.name} LIKE '{$bindingKey}%'",
              Seq(Binding(bindingKey, value))
            )
          case StartsWith(value) =>
            (
              s"${column.name} LIKE '%{$bindingKey}'",
              Seq(Binding(bindingKey, value))
            )
        }
    }
    (
      expressions.map(_._1).mkString(" AND "), //todo OR
      expressions.flatMap(_._2).toSeq
    )
  }

  def idToWhere(table: Table, spec: Spec): PartialFunction[ID, Where] = {
    case id if id.values.isEmpty => throw new UnsupportedOperationException("ID without values not supported for WHERE expression")
    case id =>
      val keys = table.pk.map(_.name)
      Where(
        id.values.map { it =>
          ColumnWhereExpression(
            table.columns.map(c => c.name -> c).toMap.apply(keys.head),
            Equals(it)
          )
        }.toSet
      )
  }

  def partToWhereExpression(table: Table): PartialFunction[Predicate[_], ColumnWhereExpression] = {
    val fieldToColumn = table.columns.map(c => c.name -> c).toMap

    def predicateFunc: PartialFunction[Predicate[_], SQLPredicate] = {
      case IntEquals(_, value) => Equals(value)
      case IntLess(_, value) => Less(value)
      case IntMore(_, value) => Greater(value)
      case IntMoreOrEquals(_, value) => GreaterOrEquals(value)
      case IntLessOrEquals(_, value) => LessOrEquals(value)

      case DoubleEquals(_, value) => Equals(value)
      case DoubleLess(_, value) => Less(value)
      case DoubleMore(_, value) => Greater(value)
      case DoubleMoreOrEquals(_, value) => GreaterOrEquals(value)
      case DoubleLessOrEquals(_, value) => LessOrEquals(value)

      case StringEquals(_, value) => Equals(value)
      case StringStartsWith(_, value) => StartsWith(value)
      case StringEndsWith(_, value) => EndsWith(value)
      case StringContains(_, value) => Contains(value)

      case IsTrue(_) => Equals("true")
      case IsFalse(_) => Equals("false")

      case IsEmpty(_) => IsNull
      case IsDefined(_) => NotNull
      case OptionValuePredicate(_, underlying) => predicateFunc(underlying)
    }

    val columnFunc: PartialFunction[Predicate[_], Column] = {
      case field: FieldPredicate[_] => fieldToColumn(field.fieldName)
    }

    {
      case part => ColumnWhereExpression(
        columnFunc(part),
        predicateFunc(part)
      )
    }
  }

}

private[sql] case class Binding(key: String, value: Any)

object Binding {
  def toScalaLikeSQL(binding: Binding): (Symbol, Any) = Symbol(binding.key) -> binding.value
}

case class SqlEntity(values: Seq[SqlValue])
case class SqlValue(column: Column, value: Any)