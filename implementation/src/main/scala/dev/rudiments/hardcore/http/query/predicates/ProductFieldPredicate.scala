package dev.rudiments.hardcore.http.query.predicates

import scala.util.matching.Regex


case class ProductFieldPredicate(override val fieldName: String, underlying: FieldPredicate[_]) extends FieldPredicate[Product]

object ProductFieldPredicate {
  private val regexp: Regex = "(\\w+).(\\w+)=(\\w+)".r

  def create(from: String, underlyingPredicate: FieldPredicate[_]): Option[ProductFieldPredicate] = {
    regexp.findFirstMatchIn(from).map { value =>
      ProductFieldPredicate(value.group(1), underlyingPredicate)
    }
  }
}