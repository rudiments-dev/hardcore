package dev.rudiments.db

import dev.rudiments.db.registry.{Column, Table}
import dev.rudiments.hardcore._
import dev.rudiments.hardcore.types.DTO

package object sql {
  case class Query(
    select: SelectExpression,
    from: FromExpression,
    where: WhereExpression,
    orderBy: OrderByExpression
  ) extends Command

  sealed trait FromExpression
  case class TableExpression(
    table: Table, as: String
  ) extends FromExpression with DTO

  sealed trait SelectExpression
  case object SelectAll extends SelectExpression
  case class SelectAllTable(table: TableExpression) extends SelectExpression
  case class SelectColumnExpression(
    table: TableExpression, column: Column, as: String
  ) extends SelectExpression

  sealed trait WhereExpression
  case class WhereColumnExpression(
    table: TableExpression, column: Column, predicate: Predicate
  ) extends WhereExpression with DTO

  sealed trait Predicate extends DTO //TODO extend from case column predicate to any valid predicates
  case object IsNull extends Predicate
  case object NotNull extends Predicate
  case class Equals(value: Any) extends Predicate
  case class NotEquals(value: Any) extends Predicate
  case class In(in: Seq[Any]) extends Predicate
  case class Greater(value: Any) extends Predicate
  case class GreaterOrEquals(value: Any) extends Predicate
  case class Lesser(value: Any) extends Predicate
  case class LesserOrEquals(value: Any) extends Predicate
  case class Between(from: Any, to: Any) extends Predicate

  sealed trait OrderByExpression
  case class OrderByColumnExpression(table: TableExpression, column: Column, order: SortOrder) extends OrderByExpression with DTO

  sealed trait SortOrder
  case object Asc extends SortOrder
  case object Desc extends SortOrder


  sealed trait QueryResult extends Event
  case class ResultSet(
    values: Seq[Map[String, Any]]
  ) extends QueryResult
  case class QueryError(e: Throwable) extends QueryResult with Error
}
