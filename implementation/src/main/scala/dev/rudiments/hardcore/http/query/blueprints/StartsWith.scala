package dev.rudiments.hardcore.http.query.blueprints

import scala.util.matching.Regex

case class StartsWith(override val fieldName: String, override val value: String) extends PredicateBlueprint[String]

object StartsWith {
  private val regexp: Regex = "(\\w+)=starts:(.*)".r

  def create(from: String)(implicit tr: TypeTransformers.Transformer[String, String]): Option[StartsWith] = {
    PredicateBlueprint.create[String, StartsWith](from, regexp)(StartsWith.apply)
  }
}