package dev.rudiments.hardcore.http.query.predicates

import scala.util.matching.Regex

case class StringStartsWith(override val fieldName: String, value: String) extends FieldPredicate[String]
case class StringEndsWith(override val fieldName: String, value: String) extends FieldPredicate[String]
case class StringContains(override val fieldName: String, value: String) extends FieldPredicate[String]

object StringStartsWith {
  private val regexp: Regex = "(\\w+)=starts:(.*)".r

  def create(from: String)(implicit tr: TypeTransformers.Transformer[String, String]): Option[StringStartsWith] = {
    FieldPredicate.create[String, StringStartsWith](from, regexp)(StringStartsWith.apply)
  }
}

object StringEndsWith {
  private val regexp: Regex = "(\\w+)=ends:(.*)".r

  def create(from: String)(implicit tr: TypeTransformers.Transformer[String, String]): Option[StringEndsWith] = {
    FieldPredicate.create[String, StringEndsWith](from, regexp)(StringEndsWith.apply)
  }
}

object StringContains {
  private val regexp: Regex = "(\\w+)=contains:(.*)".r

  def create(from: String)(implicit tr: TypeTransformers.Transformer[String, String]): Option[StringContains] = {
    FieldPredicate.create[String, StringContains](from, regexp)(StringContains.apply)
  }
}