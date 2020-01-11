package dev.rudiments.hardcore.http.query.blueprints

import scala.reflect.runtime.universe.TypeTag
import scala.util.matching.Regex

trait Predicate[T] {
  val fieldName: String
}

abstract class FieldPredicate[T] extends Predicate[T] {
  val value: T
}

object FieldPredicate {
  import TypeTransformers._
  def create[T, F <: FieldPredicate[T]](from: String, regex: Regex)(func: (String, T) => F)
                                       (implicit tr: TypeTransformers.Transformer[String, T]):Option[F] = {
    regex.findFirstMatchIn(from).map { value =>
      func(value.group(1), tr.transform(value.group(2)))
    }
  }
}
