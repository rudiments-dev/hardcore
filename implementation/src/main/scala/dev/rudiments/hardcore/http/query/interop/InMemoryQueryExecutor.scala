package dev.rudiments.hardcore.http.query.interop


import dev.rudiments.hardcore.http.query.HttpQuery
import dev.rudiments.hardcore.http.query.predicates.{FieldPredicate, IntEquals, IntLess, IntMore, IsDefined, IsEmpty, OptionValuePredicate, Predicate, ProductFieldPredicate, StartsWith, StringEquals}
import dev.rudiments.hardcore.types.{DTO, SoftInstance}


object InMemoryQueryExecutor {

  def apply(query: HttpQuery)(input: Seq[SoftInstance]): Seq[SoftInstance]  = {
    val predicates = query.parts.map {
      case predicate: FieldPredicate[_] => dto: SoftInstance => {
        val value = dto.fields(predicate.fieldName)
        if (fieldFunctions(value)(predicate)) {
          Some(dto)
        } else None
      }
      case _: Predicate[_] => throw new NotImplementedError("")
    }
    val pure: SoftInstance => Option[SoftInstance] = { dto: SoftInstance => Some(dto) }
    val function = predicates.foldLeft(pure) { case (acc, f) =>
      dto: SoftInstance => acc(dto).flatMap(f.apply)
    }

    input.flatMap(dto => function(dto))
  }

  def fieldFunctions(param: Any): PartialFunction[Predicate[_], Boolean] = {
    case IntEquals(_, value) => param.asInstanceOf[Int] == value
    case IntLess(_, value) => param.asInstanceOf[Int] < value
    case IntMore(_, value) => param.asInstanceOf[Int] > value
    case StringEquals(_, value) => param.asInstanceOf[String] == value
    case StartsWith(_, value) => param.asInstanceOf[String].startsWith(value)
    case IsEmpty(_) => param.asInstanceOf[Option[_]].isEmpty
    case IsDefined(_) => param.asInstanceOf[Option[_]].isDefined
    case OptionValuePredicate(_, underlying) => param.asInstanceOf[Option[_]].exists(value => fieldFunctions(value)(underlying))
    case ProductFieldPredicate(_, underlying) =>
      val field = param.getClass.getDeclaredField(underlying.fieldName)
      field.setAccessible(true)
      fieldFunctions(field.get(param))(underlying)
  }
}
