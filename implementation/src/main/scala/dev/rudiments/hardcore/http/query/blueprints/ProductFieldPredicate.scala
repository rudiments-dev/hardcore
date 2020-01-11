package dev.rudiments.hardcore.http.query.blueprints

import scala.util.matching.Regex


case class ProductFieldPredicate(fieldName: String, underlying: Predicate[_]) extends Predicate[Product]

object ProductFieldPredicate {
  private val regexp: Regex = "(\\w+).(\\w+)=(\\w+)".r

  def create(from: String, underlyingPredicate: Predicate[_]): Option[ProductFieldPredicate] = {
    regexp.findFirstMatchIn(from).map { value =>
      ProductFieldPredicate(value.group(1), underlyingPredicate)
    }
  }
}