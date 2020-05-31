package dev.rudiments.hardcore.http.query.predicates

import scala.util.matching.Regex

case class StartsWith(override val fieldName: String, value: String) extends FieldPredicate[String]
case class EndsWith(override val fieldName: String, value: String) extends FieldPredicate[String]
case class Contains(override val fieldName: String, value: String) extends FieldPredicate[String]

object StartsWith {
  private val regexp: Regex = "(\\w+)=starts:(.*)".r

  def create(from: String)(implicit tr: TypeTransformers.Transformer[String, String]): Option[StartsWith] = {
    FieldPredicate.create[String, StartsWith](from, regexp)(StartsWith.apply)
  }
}

object EndsWith {
  private val regexp: Regex = "(\\w+)=ends:(.*)".r

  def create(from: String)(implicit tr: TypeTransformers.Transformer[String, String]): Option[EndsWith] = {
    FieldPredicate.create[String, EndsWith](from, regexp)(EndsWith.apply)
  }
}

object Contains {
  private val regexp: Regex = "(\\w+)=contains:(.*)".r

  def create(from: String)(implicit tr: TypeTransformers.Transformer[String, String]): Option[Contains] = {
    FieldPredicate.create[String, Contains](from, regexp)(Contains.apply)
  }
}