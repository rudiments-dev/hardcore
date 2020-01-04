package dev.rudiments.hardcore.http.query.blueprints

import scala.reflect.runtime.universe.TypeTag
import scala.util.matching.Regex

trait PredicateBlueprint[T] {
  val fieldName: String
  val value: T
}

object PredicateBlueprint {
  import TypeTransformers._
  def create[T, F <: PredicateBlueprint[T]](from: String, regex: Regex)(func: (String, T) => F)
                                           (implicit tr: TypeTransformers.Transformer[String, T]):Option[F] = {
    regex.findFirstMatchIn(from).map { value =>
      func(value.group(1), tr.transform(value.group(2)))
    }
  }
}
