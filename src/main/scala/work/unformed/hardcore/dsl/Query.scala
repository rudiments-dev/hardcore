package work.unformed.hardcore.dsl

import enumeratum._

import scala.collection.immutable

case class Query[A](
  filters: Seq[Filter[A]] = Seq.empty,
  sorting: Seq[Sort[A]] = Seq.empty,
  page: Paging = Paging.default
)

case class Filter[A](field: String, condition: Predicate)

case class Sort[A](field: String, order: SortOrder)

sealed trait Paging
object Paging {
  val default: Paging = Page(100, 0)
}
case class Page(limit: Long, offset: Long) extends Paging
case object All extends Paging


sealed trait Predicate

case object Empty extends Predicate
case object NotEmpty extends Predicate
case class  Equals(value: Any) extends Predicate
case class  NotEquals(value: Any) extends Predicate
case class  In(in: Seq[Any]) extends Predicate
case class  Greater(value: Any) extends Predicate
case class  GreaterOrEquals(value: Any) extends Predicate
case class  Lesser(value: Any) extends Predicate
case class  LesserOrEquals(value: Any) extends Predicate
case class  Between(from: Any, to: Any) extends Predicate
case class  Like(pattern: String)


sealed trait SortOrder extends EnumEntry
object SortOrder extends Enum[SortOrder] {
  override def values: immutable.IndexedSeq[SortOrder] = findValues

  case object Asc extends SortOrder
  case object Desc extends SortOrder
}

case class Result[A](query: Query[A], values: Seq[A])