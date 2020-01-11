package dev.rudiments.hardcore.http.query.interop

import java.lang.reflect.Field

import dev.rudiments.hardcore.http.query.Query
import dev.rudiments.hardcore.http.query.blueprints.{IntEquals, IntLess, IntMore, IsDefined, IsEmpty, OptionValuePredicate, Predicate, ProductFieldPredicate, StartsWith, StringEquals}
import dev.rudiments.hardcore.types.DTO

import scala.reflect.ClassTag

object InMemoryQueryExecutor {

  def apply[T <: DTO : ClassTag](blueprint: Query[T])(input: Seq[T]): Seq[T]  = {
    val queries = blueprint.parts.map { part =>
      dto: T => {
        val valueFunc = getFieldValue[T](part.fieldName)
        val value = valueFunc(dto)
        if (fieldFunctions(value)(part)) {
          Some(dto)
        } else None
      }
    }
    val pure: T => Option[T] = { dto: T => Some(dto) }
    val function = queries.foldLeft(pure) { case (acc, f) =>
      dto: T => acc(dto).flatMap(f.apply)
    }

    input.flatMap(dto => function(dto))
  }

  private def getFieldValue[T: ClassTag](fieldName: String): T => Any = {
    val reflect: Field = implicitly[ClassTag[T]].runtimeClass.getDeclaredField(fieldName)
    reflect.setAccessible(true)

    dto: T => reflect.get(dto)
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

