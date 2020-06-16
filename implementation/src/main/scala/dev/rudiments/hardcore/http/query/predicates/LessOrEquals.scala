package dev.rudiments.hardcore.http.query.predicates

import scala.util.matching.Regex

private[predicates] sealed trait LessOrEquals[T] extends FieldPredicate[T]


case class IntLessOrEquals(override val fieldName: String, value: Int) extends LessOrEquals[Int]
case class DoubleLessOrEquals(override val fieldName: String, value: Double) extends LessOrEquals[Double]

object IntLessOrEquals {
  private val regexp: Regex = "(\\w+)=lessOrEq:(.*)".r

  def create(from: String)(implicit tr: TypeTransformers.Transformer[String, Int]): Option[IntLessOrEquals] = {
    FieldPredicate.create[Int, IntLessOrEquals](from, regexp)(IntLessOrEquals.apply)
  }
}

object DoubleLessOrEquals {
  private val regexp: Regex = "(\\w+)=lessOrEq:(.*)".r

  def create(from: String)(implicit tr: TypeTransformers.Transformer[String, Double]): Option[DoubleLessOrEquals] = {
    FieldPredicate.create[Double, DoubleLessOrEquals](from, regexp)(DoubleLessOrEquals.apply)
  }
}