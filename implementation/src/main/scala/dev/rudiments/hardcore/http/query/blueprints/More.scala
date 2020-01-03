package dev.rudiments.hardcore.http.query.blueprints

import scala.util.matching.Regex

private[blueprints] sealed trait MoreBlueprint[T] extends PredicateBlueprint[T]

case class IntMoreBlueprint(override val fieldName: String, override val value: Int) extends MoreBlueprint[Int]
case class DoubleMoreBlueprint(override val fieldName: String, override val value: Double) extends MoreBlueprint[Double]

object IntMoreBlueprint {
  private val regexp: Regex = "(\\w+)=more:(.*)".r

  def create(from: String)(implicit tr: TypeTransformers.Transformer[String, Int]): Option[IntMoreBlueprint] = {
    PredicateBlueprint.create[Int, IntMoreBlueprint](from, regexp)(IntMoreBlueprint.apply)
  }
}

object DoubleMoreBlueprint {
  private val regexp: Regex = "(\\w+)=more:(.*)".r

  def create(from: String)(implicit tr: TypeTransformers.Transformer[String, Double]): Option[DoubleMoreBlueprint] = {
    PredicateBlueprint.create[Double, DoubleMoreBlueprint](from, regexp)(DoubleMoreBlueprint.apply)
  }
}