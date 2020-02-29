package dev.rudiments.hardcore.http.query.interop

import java.lang.reflect.Field

import dev.rudiments.hardcore.http.query.{Query, QueryBlueprint}
import dev.rudiments.hardcore.http.query.blueprints.{IntEqualsBlueprint, IntLessBlueprint, IntMoreBlueprint, StartsWith, StringEqualsBlueprint}
import dev.rudiments.hardcore.types.DTO

import scala.reflect.ClassTag

object Compiler {

  def getFieldValue[T : ClassTag, Result](fieldName: String): T => Result = {
    val reflect: Field = implicitly[ClassTag[T]].runtimeClass.getDeclaredField(fieldName)
    reflect.setAccessible(true)

    dto: T => reflect.get(dto).asInstanceOf[Result]
  }


  def compile[T : ClassTag](blueprint: QueryBlueprint[T]): Query[T] = {
    val queries = blueprint.parts.map {
      case IntEqualsBlueprint(fieldName, value) => Query[T] { dto: T =>
        val valueFunc = getFieldValue[T, Int](fieldName)
        if (valueFunc(dto) == value) {
          Some(dto)
        } else None
      }

      case IntLessBlueprint(fieldName, value) => Query[T] { dto: T =>
        val valueFunc = getFieldValue[T, Int](fieldName)
        if (valueFunc(dto) < value) {
          Some(dto)
        } else None
      }

      case IntMoreBlueprint(fieldName, value) => Query[T] { dto: T =>
        val valueFunc = getFieldValue[T, Int](fieldName)
        if (valueFunc(dto) > value) {
          Some(dto)
        } else None
      }

      case StringEqualsBlueprint(fieldName, value) => Query[T] { dto: T =>
        val valueFunc = getFieldValue[T, String](fieldName)
        if (valueFunc(dto) == value) {
          Some(dto)
        } else None
      }

      case StartsWith(fieldName, value) => Query[T] { dto: T =>
        val valueFunc = getFieldValue[T, String](fieldName)
        if (valueFunc(dto).startsWith(value)) {
          Some(dto)
        } else None
      }
    }

    queries.foldLeft(Query.pure[T])(_.compose(_))
  }
}
