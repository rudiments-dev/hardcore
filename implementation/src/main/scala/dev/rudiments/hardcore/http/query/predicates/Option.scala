package dev.rudiments.hardcore.http.query.predicates

import dev.rudiments.hardcore.http.query.Param

import scala.util.matching.Regex

sealed trait OptionPredicate extends FieldPredicate[Option[_]]

case class OptionValuePredicate(override val fieldName: String, underlying: FieldPredicate[_]) extends OptionPredicate

case class IsEmpty(override val fieldName: String) extends OptionPredicate
case class IsDefined(override val fieldName: String) extends OptionPredicate

object OptionValuePredicate {

  def create(from: Param, underlyingPredicate: FieldPredicate[_]): Some[OptionValuePredicate] = {
    Some(
      OptionValuePredicate(from.fieldName, underlyingPredicate)
    )
  }
}

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