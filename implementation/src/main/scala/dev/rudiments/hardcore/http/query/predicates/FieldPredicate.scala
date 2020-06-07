package dev.rudiments.hardcore.http.query.predicates

import scala.reflect.runtime.universe.TypeTag
import scala.util.matching.Regex

trait Predicate[T] {}

abstract class FieldPredicate[T] extends Predicate[T] {
  val fieldName: String
}

object FieldPredicate {
  def create[T, F <: FieldPredicate[_]](from: String, regex: Regex)(func: (String, T) => F)
                                       (implicit tr: TypeTransformers.Transformer[String, T]):Option[F] = {
    regex.findFirstMatchIn(from).map { value =>
      func(value.group(1), tr.transform(value.group(2)))
    }
  }
}
