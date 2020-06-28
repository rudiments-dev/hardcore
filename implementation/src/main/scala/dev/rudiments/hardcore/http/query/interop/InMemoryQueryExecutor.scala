package dev.rudiments.hardcore.http.query.interop


import dev.rudiments.hardcore.http.query.{PassAllQuery, PredicatesQuery, Query}
import dev.rudiments.hardcore.http.query.predicates.{DoubleEquals, DoubleLess, DoubleLessOrEquals, DoubleMore, DoubleMoreOrEquals, FieldPredicate, IntEquals, IntLess, IntLessOrEquals, IntMore, IntMoreOrEquals, IsDefined, IsEmpty, OptionValuePredicate, Predicate, ProductFieldPredicate, StringContains, StringEndsWith, StringEquals, StringStartsWith}
import dev.rudiments.hardcore.types.{DTO, Instance, SoftInstance}


object InMemoryQueryExecutor {

  def apply(query: Query)(input: Seq[Instance]): Seq[Instance]  = {
    query match {
      case PassAllQuery(_) => input
      case PredicatesQuery(parts, softType) =>
        val predicates = parts.map {
          case predicate: FieldPredicate[_] => dto: Instance => {
            val value = dto.extract[Any](predicate.fieldName)
            if (fieldFunctions(value)(predicate)) {
              Some(dto)
            } else None
          }
          case other: Predicate[_] => throw new NotImplementedError(s"$other predicate is not implemented in InMemoryQueryExecutor")
        }
        val pure: Instance => Option[Instance] = { dto: Instance => Some(dto) }
        val function = predicates.foldLeft(pure) { case (acc, f) =>
          dto: Instance => acc(dto).flatMap(f.apply)
        }

        input.flatMap(dto => function(dto))
    }
  }

  def fieldFunctions(param: Any): PartialFunction[Predicate[_], Boolean] = {
    case IntEquals(_, value) => param.asInstanceOf[Int] == value
    case IntLess(_, value) => param.asInstanceOf[Int] < value
    case IntMore(_, value) => param.asInstanceOf[Int] > value
    case IntMoreOrEquals(_, value) => param.asInstanceOf[Int] >= value
    case IntLessOrEquals(_, value) => param.asInstanceOf[Int] <= value
    case DoubleEquals(_, value) => param.asInstanceOf[Double] == value
    case DoubleLess(_, value) => param.asInstanceOf[Double] < value
    case DoubleMore(_, value) => param.asInstanceOf[Double] > value
    case DoubleMoreOrEquals(_, value) => param.asInstanceOf[Double] >= value
    case DoubleLessOrEquals(_, value) => param.asInstanceOf[Double] <= value
    case StringEquals(_, value) => param.asInstanceOf[String] == value
    case StringStartsWith(_, value) => param.asInstanceOf[String].startsWith(value)
    case StringEndsWith(_, value) => param.asInstanceOf[String].endsWith(value)
    case StringContains(_, value) => param.asInstanceOf[String].contains(value)
    case IsEmpty(_) => param.asInstanceOf[Option[_]].isEmpty
    case IsDefined(_) => param.asInstanceOf[Option[_]].isDefined
    case OptionValuePredicate(_, underlying) => param.asInstanceOf[Option[_]].exists(value => fieldFunctions(value)(underlying))
    case ProductFieldPredicate(_, underlying) =>
      fieldFunctions(
        param.asInstanceOf[Instance].extract[Any](underlying.fieldName)
      )(underlying)
  }
}
