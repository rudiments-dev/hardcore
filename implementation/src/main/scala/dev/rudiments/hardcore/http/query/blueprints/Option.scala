package dev.rudiments.hardcore.http.query.blueprints

import dev.rudiments.hardcore.http.query.blueprints.StringEqualsBlueprint.regexp

import scala.util.matching.Regex

trait OptionPredicate extends PredicateBlueprint[Option[_]]

case class ValuePredicate(underlying: FieldPredicateBlueprint[_]) extends OptionPredicate

case class IsEmpty(fieldName: String) extends OptionPredicate
case class IsDefined(fieldName: String) extends OptionPredicate

object IsEmpty {
  private val regexp: Regex = "(\\w+)=empty".r

  def create(from: String): Option[IsEmpty] = {
    regexp.findFirstMatchIn(from).map { value =>
      IsEmpty(value.group(1))
    }
  }
}

object IsDefined {
  private val regexp: Regex = "(\\w+)=defined".r

  def create(from: String): Option[IsDefined] = {
    regexp.findFirstMatchIn(from).map { value =>
      IsDefined(value.group(1))
    }
  }
}