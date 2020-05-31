package dev.rudiments.hardcode.sql.materializer

import dev.rudiments.hardcode.sql.SQL
import dev.rudiments.hardcode.sql.SQLDataClasses.{DeleteDataClass, FindByIdDataClass, InsertDataClass, UpdateDataClass}
import dev.rudiments.hardcode.sql.SQLParts._
import dev.rudiments.hardcode.sql.SQLPredicates._
import dev.rudiments.hardcode.sql.scalalike.{CreateSQL, DropSQL, FindByIDSQL, UpdateSQL}

trait SQLMaterializer[T <: SQL[_]] {

  final def selectPart(select: Select): String = select.selectors.map {
    case Selector(column, Some(as)) => s"${column.name} as $as"
    case Selector(column, None) => column.name
  }.mkString(", ")

  final def fromPart(from: From): String = from match {
    case From(schema, table, Some(as)) => s"${schema.name}.${table.name} as $as"
    case From(schema, table, None) => s"${schema.name}.${table.name}"
  }

  final def wherePart(where: Where): (String, Set[Binding]) = {
    val expressions = where.expressions.map {
      case exp@ColumnWhereExpression(column, predicate) =>
        val code = exp.hashCode().toString
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
          case Lesser(value) =>
            (
              s"${column.name} < {$bindingKey}",
              Seq(Binding(bindingKey, value))
            )
          case LesserOrEquals(value) =>
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
        }
    }
    (
      expressions.map(_._1).mkString(" AND "), //todo OR
      expressions.flatMap(_._2)
    )
  }

  def insertSQL(insert: InsertDataClass): CreateSQL

  def findByIdSQL(findById: FindByIdDataClass): FindByIDSQL

  def dropSQL(delete: DeleteDataClass): DropSQL

  def updateSQL(update: UpdateDataClass): UpdateSQL
}

private[sql] case class Binding(key: String, value: Any)
