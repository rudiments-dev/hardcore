package dev.rudiments.hardcore.http.query.blueprints

import scala.util.matching.Regex

private[blueprints] sealed trait LessBlueprint[T] extends FieldPredicateBlueprint[T]

case class IntLessBlueprint(override val fieldName: String, override val value: Int) extends LessBlueprint[Int]
case class DoubleLessBlueprint(override val fieldName: String, override val value: Double) extends LessBlueprint[Double]

object IntLessBlueprint {
  private val regexp: Regex = "(\\w+)=less:(.*)".r

  def create(from: String)(implicit tr: TypeTransformers.Transformer[String, Int]): Option[IntLessBlueprint] = {
    FieldPredicateBlueprint.create[Int, IntLessBlueprint](from, regexp)(IntLessBlueprint.apply)
  }
}

object DoubleLessBlueprint {
  private val regexp: Regex = "(\\w+)=less:(.*)".r

  def create(from: String)(implicit tr: TypeTransformers.Transformer[String, Double]): Option[DoubleLessBlueprint] = {
    FieldPredicateBlueprint.create[Double, DoubleLessBlueprint](from, regexp)(DoubleLessBlueprint.apply)
  }
}