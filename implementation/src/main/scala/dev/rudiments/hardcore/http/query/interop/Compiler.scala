package dev.rudiments.hardcore.http.query.interop

import java.lang.reflect.Field

import dev.rudiments.hardcore.http.query.{Query, QueryBlueprint}
import dev.rudiments.hardcore.http.query.blueprints.{IntEqualsBlueprint, IntLessBlueprint, IntMoreBlueprint, IsDefined, IsEmpty, PredicateBlueprint, StartsWith, StringEqualsBlueprint, ValuePredicate}
import dev.rudiments.hardcore.types.DTO

import scala.reflect.ClassTag

object Compiler {

  def getFieldValue[T : ClassTag, Result](fieldName: String): T => Result = {
    val reflect: Field = implicitly[ClassTag[T]].runtimeClass.getDeclaredField(fieldName)
    reflect.setAccessible(true)

    dto: T => reflect.get(dto).asInstanceOf[Result]
  }


  def compile[T <: DTO : ClassTag](blueprint: QueryBlueprint[T]): Query[T] = {

    val queries = blueprint.parts.map {
      fieldPredicate[T].orElse(optionPredicate[T])
    }.map(Query.apply)

    queries.foldLeft(Query.pure[T])(_.compose(_))
  }

  def fieldPredicate[T: ClassTag]: PartialFunction[PredicateBlueprint[_], T => Option[T]]  = {
    case IntEqualsBlueprint(fieldName, value) => dto: T =>
      val valueFunc = getFieldValue[T, Int](fieldName)
      if (valueFunc(dto) == value) {
        Some(dto)
      } else None

    case IntLessBlueprint(fieldName, value) => dto: T =>
      val valueFunc = getFieldValue[T, Int](fieldName)
      if (valueFunc(dto) < value) {
        Some(dto)
      } else None

    case IntMoreBlueprint(fieldName, value) => dto: T =>
      val valueFunc = getFieldValue[T, Int](fieldName)
      if (valueFunc(dto) > value) {
        Some(dto)
      } else None

    case StringEqualsBlueprint(fieldName, value) => dto: T =>
      val valueFunc = getFieldValue[T, String](fieldName)
      if (valueFunc(dto) == value) {
        Some(dto)
      } else None

    case StartsWith(fieldName, value) => dto: T =>
      val valueFunc = getFieldValue[T, String](fieldName)
      if (valueFunc(dto).startsWith(value)) {
        Some(dto)
      } else None
  }

  def optionPredicate[T : ClassTag]: PartialFunction[PredicateBlueprint[_], T => Option[T]] = {
    case IsEmpty(fieldName) => dto: T =>
      val valueFunc = getFieldValue[T, Option[_]](fieldName)
      if (valueFunc(dto).isEmpty) {
        Some(dto)
      } else None

    case IsDefined(fieldName) => dto: T =>
      val valueFunc = getFieldValue[T, Option[_]](fieldName)
      if (valueFunc(dto).isDefined) {
        Some(dto)
      } else None

    case ValuePredicate(underlyingPredicate) => dto: T =>
      val valueFunc = getFieldValue[T, Option[_]](underlyingPredicate.fieldName)
      valueFunc(dto).flatMap { value =>
        if(fieldFunctions(value)(underlyingPredicate)) {
          Some(dto)
        } else None
      }

  }

  def fieldFunctions(param: Any): PartialFunction[PredicateBlueprint[_], Boolean] = {
    case IntEqualsBlueprint(fieldName, value) =>
      param.asInstanceOf[Int] == value
    case IntLessBlueprint(fieldName, value) =>
      param.asInstanceOf[Int] < value
    case IntMoreBlueprint(fieldName, value) =>
      param.asInstanceOf[Int] > value
    case StringEqualsBlueprint(fieldName, value) =>
      param.asInstanceOf[String] == value
    case StartsWith(fieldName, value) =>
      param.asInstanceOf[String].startsWith(value)
  }
}
