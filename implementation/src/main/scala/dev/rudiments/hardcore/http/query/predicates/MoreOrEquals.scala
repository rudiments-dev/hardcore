package dev.rudiments.hardcore.http.query.predicates


import scala.util.matching.Regex

private[predicates] sealed trait MoreOrEquals[T] extends FieldPredicate[T]


case class IntMoreOrEquals(override val fieldName: String, value: Int) extends MoreOrEquals[Int]
case class DoubleMoreOrEquals(override val fieldName: String, value: Double) extends MoreOrEquals[Double]

object IntMoreOrEquals {
  private val regexp: Regex = "(\\w+)=moreOrEq:(.*)".r

  def create(from: String)(implicit tr: TypeTransformers.Transformer[String, Int]): Option[IntMoreOrEquals] = {
    FieldPredicate.create[Int, IntMoreOrEquals](from, regexp)(IntMoreOrEquals.apply)
  }
}

object DoubleMoreOrEquals {
  private val regexp: Regex = "(\\w+)=moreOrEq:(.*)".r

  def create(from: String)(implicit tr: TypeTransformers.Transformer[String, Double]): Option[DoubleMoreOrEquals] = {
    FieldPredicate.create[Double, DoubleMoreOrEquals](from, regexp)(DoubleMoreOrEquals.apply)
  }
}