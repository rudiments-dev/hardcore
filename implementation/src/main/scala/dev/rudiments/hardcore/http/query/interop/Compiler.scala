package dev.rudiments.hardcore.http.query.interop

import java.lang.reflect.Field

import dev.rudiments.hardcore.http.query.{Query, QueryBlueprint}
import dev.rudiments.hardcore.http.query.blueprints.{IntEqualsBlueprint, IntLessBlueprint, IntMoreBlueprint, IsDefined, IsEmpty, OptionValuePredicate, PredicateBlueprint, ProductFieldPredicate, StartsWith, StringEqualsBlueprint}
import dev.rudiments.hardcore.types.DTO

import scala.reflect.ClassTag

object Compiler {

  def compile[T <: DTO : ClassTag](blueprint: QueryBlueprint[T]): Query[T] = {

    val queries = blueprint.parts.map { part =>
      dto: T => {
        val valueFunc = getFieldValue[T](part.fieldName)
        val value = valueFunc(dto)
        if (fieldFunctions(value)(part)) {
          Some(dto)
        } else None
      }
    }.map(Query.apply)
    queries.foldLeft(Query.pure[T])(_.compose(_))
  }

  private def getFieldValue[T: ClassTag](fieldName: String): T => Any = {
    val reflect: Field = implicitly[ClassTag[T]].runtimeClass.getDeclaredField(fieldName)
    reflect.setAccessible(true)

    dto: T => reflect.get(dto)
  }

  def fieldFunctions(param: Any): PartialFunction[PredicateBlueprint[_], Boolean] = {
    case IntEqualsBlueprint(_, value) => param.asInstanceOf[Int] == value
    case IntLessBlueprint(_, value) => param.asInstanceOf[Int] < value
    case IntMoreBlueprint(_, value) => param.asInstanceOf[Int] > value
    case StringEqualsBlueprint(_, value) => param.asInstanceOf[String] == value
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

