package dev.rudiments.hardcore.http.query.predicates

import scala.util.matching.Regex

case class StartsWith(override val fieldName: String, value: String) extends FieldPredicate[String]

object StartsWith {
  private val regexp: Regex = "(\\w+)=starts:(.*)".r

  def create(from: String)(implicit tr: TypeTransformers.Transformer[String, String]): Option[StartsWith] = {
    FieldPredicate.create[String, StartsWith](from, regexp)(StartsWith.apply)
  }
}