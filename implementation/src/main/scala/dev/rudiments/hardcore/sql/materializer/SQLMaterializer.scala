package dev.rudiments.hardcore.sql.materializer


import dev.rudiments.hardcore.sql.SQL
import dev.rudiments.hardcore.sql.parts.{Between, ColumnWhereExpression, Equals, From, Greater, GreaterOrEquals, In, IsNull, Lesser, LesserOrEquals, NotEquals, NotNull, Select, Selector, Where}

trait SQLMaterializer[T <: SQL[_]] {

  final def selectPart(select: Select): String = select.selectors.map {
    case Selector(column, Some(as)) => s"${column.name} as $as"
    case Selector(column, None) => column.name
  }.mkString(", ")

  final def fromPart(from: From): String = from match {
    case From(table, Some(as)) => s"${table.name} as $as"
    case From(table, None) => table.name
  }

  final def wherePart(where: Where): (String, Set[Binding]) = {
    val expressions = where.expressions.map {
      case exp@ColumnWhereExpression(column, predicate) =>
        val code = exp.hashCode().toString
        val bindingKey = s"${code}_$column"
        predicate match {
          case IsNull =>
            (
              s"$column IS NULL",
              Seq.empty
            )
          case NotNull =>
            (
              s"$column IS NOT NULL",
              Seq.empty
            )
          case Equals(value) =>
            (
              s"$column = {$bindingKey}",
              Seq(Binding(bindingKey, value))
            )
          case NotEquals(value) =>
            (
              s"$column != {$bindingKey}",
              Seq(Binding(bindingKey, value))
            )
          case In(in) =>
            val bindings = in.zipWithIndex.map { case (value, i) => Binding(s"${bindingKey}_$i", value)}
            (
              s"$column IN {${bindings.map(_.key).mkString(", ")}}",
              bindings
            )
          case Greater(value) =>
            (
              s"$column > {$bindingKey}",
              Seq(Binding(bindingKey, value))
            )
          case GreaterOrEquals(value) =>
            (
              s"$column >= {$bindingKey}",
              Seq(Binding(bindingKey, value))
            )
          case Lesser(value) =>
            (
              s"$column < {$bindingKey}",
              Seq(Binding(bindingKey, value))
            )
          case LesserOrEquals(value) =>
            (
              s"$column <= {$bindingKey}",
              Seq(Binding(bindingKey, value))
            )
          case Between(from, to) =>
            val left = s"${bindingKey}_left"
            val right = s"${bindingKey}_right"

            (
              s"$column BETWEEN {$left} AND {$right}",
              Seq(Binding(left, from), Binding(right, to))
            )
        }
    }
    (
      expressions.map(_._1).mkString(" AND "), //todo OR
      expressions.flatMap(_._2)
    )
  }


}

private[sql] case class Binding(key: String, value: Any)
